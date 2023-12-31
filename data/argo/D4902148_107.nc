CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       ~2017-07-22T15:37:17Z creation;2017-07-22T15:37:22Z conversion to V3.1;2019-12-18T07:29:20Z update;2022-11-21T05:32:16Z update;     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         BPRIMARY|https://orcid.org/0000-0001-9150-6442|Kanako Sato, JAMSTEC        @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    7H   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    7X   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    7\   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    7`   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7p   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7�   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  8   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        8H   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    8L   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    8P   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     8T   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8t   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8x   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8|   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8�   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8�   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8�   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    9   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    9   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    9   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        :   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  :    PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ]   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  a    TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tp   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �l   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �P   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �L   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �d   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �t   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �x   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �|   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20170722153717  20221123111507  4902148 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               kA   JA  I1_0397_107                     2C  Dd@�NAVIS_A                         0397                            ARGO 011514                     863 @��v��1   @���b��@;�?|�h�d@�c�	1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @���@�  A   A   A@  A`  A�  A�  A�  A�  A�33A���A�  A�  A�33B  B  B  B   B(  B0  B8  B@  BH  BP  BXffB`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@fD@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DGfDG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[fD[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dw��Dx� Dy  Dy� Dz  Dzy�D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̓3D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D��3D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�� 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111
@�
=@�p�@�p�A�RA>�RA^�RA~�RA�\)A�\)A�\)A��\A�(�A�\)A�\)A��\B�B�B�B�B'�B/�B7�B?�BG�BO�BX{B_�Bg�Bo�Bw�B�B��
B��
B��
B��
B��
B��
B��
B��
B��
B��
B��
B��
B��
B��
B��
B��
B��
B��
B��
B��
B��
B��
B��
B��
B��
B��
B��
B��
B��
B��
B��
B��
C�C�C�C�C	�C�C�C�C�C�C�C�C�C�C�C�C!�C#�C%�C'�C)�C+�C-�C/�C1�C3�C5�C7�C9�C;�C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D z�D ��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��D	z�D	��D
z�D
��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��D z�D ��D!z�D!��D"z�D"��D#z�D#��D$z�D$��D%z�D%��D&z�D&��D'z�D'��D(z�D(��D)z�D)��D*z�D*��D+z�D+��D,z�D,��D-z�D-��D.z�D.��D/z�D/��D0z�D0��D1z�D1��D2z�D2��D3z�D3��D4z�D4��D5z�D5��D6z�D6��D7z�D7��D8z�D8��D9z�D9��D:z�D:��D;z�D;��D<z�D<��D=z�D=��D>z�D>��D?z�D@HD@z�D@��DAz�DA��DBz�DB��DCz�DC��DDz�DD��DEz�DE��DFz�DGHDGz�DG��DHz�DH��DIz�DI��DJz�DJ��DKz�DK��DLz�DL��DMz�DM��DNz�DN��DOz�DO��DPz�DP��DQz�DQ��DRz�DR��DSz�DS��DTz�DT��DUz�DU��DVz�DV��DWz�DW��DXz�DX��DYz�DY��DZz�D[HD[z�D[��D\z�D\��D]z�D]��D^z�D^��D_z�D_��D`z�D`��Daz�Da��Dbz�Db��Dcz�Dc��Ddz�Dd��Dez�De��Dfz�Df��Dgz�Dg��Dhz�Dh��Diz�Di��Djz�Dj��Dkz�Dk��Dlz�Dl��Dmz�Dm��Dnz�Dn��Doz�Do��Dpz�Dp��Dqz�Dq��Drz�Dr��Dsz�Ds��Dtz�Dt��Duz�Du��Dvz�Dv��Dwz�Dw�{Dxz�Dx��Dyz�Dy��Dzt{Dz��D{z�D{��D|z�D|��D}z�D}��D~z�D~��Dz�D��D�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��=D��qD�=qD�}qD��qD��=D�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD½qD��qD�=qD�}qDýqD��qD�=qD�}qDĽqD��qD�=qD�}qDŽqD��qD�=qD�}qDƽqD��qD�=qD�}qDǽqD��qD�=qD�}qDȽqD��qD�=qD�}qDɽqD��qD�=qD�}qDʽqD��qD�=qD�}qD˽qD��qD�=qD�}qD̽qD��qD�=qD̀�DͽqD��qD�=qD�}qDνqD��qD�=qD�}qDϽqD��qD�=qD�}qDнqD��qD�=qD�}qDѽqD��qD�=qD�}qDҽqD��qD�=qD�}qDӽqD��qD�=qD�}qDԽqD��qD�=qD�}qDսqD��qD�=qD�}qDֽqD��qD�=qD�}qD׽qD��qD�=qD�}qDؽqD��qD�=qD�}qDٽqD��qD�=qD�}qDڽqD��qD�=qD�}qD۽qD��qD�=qD�}qDܽqD��qD�=qD�}qDݽqD��qD�=qD�}qD޽qD��qD�=qD�}qD߽qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD��qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD���D� �D�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��q11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111�A�$�A�&�A�&�A�&�A�+A�(�A�+A�-A�
=AӸRA�Q�A�33A���A�=qA�  A�O�A�{A�7LA��-A��\A�XA���A���A���A�;dA��A��^A��A��A�dZA�A��A���A�"�A��yA�(�A�ȴA���A�XA��mA���A�9XA��A�jA�$�A���A���A��A�z�A�5?A��A���A�VA�jA��;A�E�A�  A���A���A�ȴA�bNA�XAC�A~�!A~{A}��A|ĜAyx�Au�#Ar��Ao�TAm�Alr�Ak��Ak�Aj9XAi&�Ah��Ah=qAg�
Af�Ad�!Ad5?Ac�hAbĜAbbNAa��Aa7LA`ffA_��A_XA]�-A\Q�A[�AZ�uAYx�AX$�AWXAV^5AU�AU�PAU7LAT{ASS�AR$�AQ�APQ�AOl�AN�DAMS�AK33AI��AI��AI33AH�AHQ�AGt�AG7LAF�/AFM�AE/ADQ�AD-ACƨACt�AC�AB��AB��ABA�AA�AA�TAA�;AA�#AA��AAhsA?��A>�/A=%A;�;A;;dA:�DA:bA9��A9�hA8�A89XA7�A6��A6=qA41A3XA2�A2��A2A�A1\)A0��A/�A/\)A/
=A.ȴA.VA-ƨA-K�A,��A+�;A+?}A*�DA)�mA)�A(�A(A'S�A&��A&bA%|�A%�A$�\A$5?A$JA#�TA#�FA#��A#��A#t�A#C�A#/A#
=A"��A"bNA!�A  A�HA��A �A��A��A�AVA�A��AbNA{A��A�A1A`BA�A�/A�A��A�`AVA(�A|�A��A��A&�A�A5?A��A	�mA	�PA	`BAĜAƨA5?AƨA��AS�A�/A��A
=AȴA��AZA  A�FA�A%A Z@�dZ@�;d@��@�hs@�z�@���@�b@�
=@��!@�=q@���@�"�@�@�A�@�X@�!@�x�@�D@��@�-@��m@��@���@��@�z�@� �@�~�@��;@��T@أ�@�7L@�9X@���@�\)@�$�@���@Ϯ@ϕ�@�C�@�ȴ@ͺ^@�I�@�+@�^5@ɑh@��@�r�@�\)@ģ�@��#@�K�@��@��/@��j@�K�@��#@��@�
=@�x�@�Q�@�1@��
@���@�l�@�C�@�
=@��H@���@�@�&�@��@�z�@�A�@�ƨ@�J@���@��@���@�t�@�ff@��@��9@�
=@��@�V@�9X@���@�S�@��@�5?@�%@��@�1'@��m@��R@�ff@���@�@��j@�o@��R@�n�@�$�@��@��@��-@�7L@�%@��@��@�bN@�  @�t�@�~�@�hs@�Ĝ@���@�@��H@���@��+@�5?@��^@���@�?}@��@�1@���@��@�x�@���@��@�Z@���@��;@��m@�C�@���@���@�{@���@���@�S�@�+@���@���@��`@�  @���@�t�@�K�@�"�@��@���@��!@��+@�E�@�@��7@�p�@�p�@�hs@�X@�O�@�G�@�/@�V@���@��`@��j@��D@�Z@�I�@�1'@�1'@�1'@�b@��@~�@~V@~E�@~$�@~@}�T@}�@}/@|�@|9X@{��@{�@{t�@{dZ@{S�@z�H@y�@yG�@x�`@x��@x�9@x�u@xr�@x �@w�@wK�@v��@v�y@v��@vE�@v5?@v$�@v@u�@u��@u��@u��@u�h@u`B@t�@t9X@s@rJ@p�9@p  @oK�@n�R@m`B@m�@lZ@l1@k�F@k��@kt�@k33@j�@j�!@j^5@i��@i7L@h��@hbN@g|�@g;d@g�@f��@f�R@f�+@fv�@fV@f5?@f$�@f@e�h@e/@d�j@d��@d�D@dz�@c�m@b�H@b�@a&�@a%@`�9@`1'@`b@`b@`b@` �@`b@`b@`b@`b@` �@`b@`b@`  @_�;@_�w@_;d@^ȴ@]�h@\��@\�j@\�j@\�j@\�D@]V@\��@\�@\��@\�/@\�j@\z�@\Z@\9X@[��@Zn�@Y7L@YG�@YX@Z=q@Z-@Zn�@Z^5@Z=q@Y��@Y�^@Y�^@Y�7@Yhs@X��@X�@X��@X��@Xr�@XbN@XbN@Xr�@XA�@X  @W�@Vv�@Up�@Up�@U?}@T��@T��@T9X@S��@S�m@S�
@S�F@S�@S33@S33@S"�@R��@R~�@RM�@Q��@Q&�@P��@PbN@O�w@N�y@N@Mp�@L��@Lj@L�@K��@K�F@KdZ@K33@Ko@JM�@I�@H��@HbN@Hb@G�@G�w@G�P@G\)@G\)@GK�@G�@F��@F��@Fv�@F$�@F{@F@E�@E�T@E�T@E��@E@E�-@EO�@EV@D�/@D�@D��@D�@D�@C��@CdZ@CS�@CC�@B�!@BJ@A��@AX@@�`@@��@@�@@�@@�@@bN@@bN@@ �@@ �@@b@@  @?�@?�;@?�@?|�@?K�@?;d@?�@>��@>ȴ@>��@>��@>�+@>ff@>V@>E�@>$�@=�h@=p�@=V@<�@<�D@<z�@<z�@<z�@<z�@<Z@;�m@;�m@;�m@;��@;S�@;"�@:�H@:�\@:J@9��@9��@9��@9�7@9&�@8�u@7�;@7l�@6�@6��@6�+@6v�@6E�@5�@5��@5��@5@5�@5�@4�@4��@4�@4Z@4�@333@2�@2�!@2��@2�\@2M�@2-@2-@2-@2�@2�@1��@1x�@0�`@0�u@0A�@0b@/��@/�P@/\)@/K�@/;d@.��@.�@.��@.�+@.v�@.ff@.E�@.5?@.E�@.5?@.{@-�h@,��@,��@,��@,I�@,(�@,�@,1@,1@+ƨ@+��@+dZ@*�@*^5@*-@*�@*�@*J@)%@(Ĝ@(r�@(r�@(1'@(b@(  @(  @(  @'�@'�@'�;@'�w@'�P@'l�@'\)@'�@&ff@%�-@%�h@%�h@%�@%`B@%?}@$I�@"M�@!��@!hs@!X@!G�@!G�@ ��@ �`@ �@ r�@  �@�w@|�@|�@�@�y@�y@�y@�@�y@v�@v�@ff@V@$�@{@@{@ff@E�@�-@��@@��@O�@O�@O�@��@z�@�@(�@�
@�F@�F@��@��@��@t�@t�@t�@t�@dZ@C�@"�@�^@��@ �@��@�P@
=@�@��@V@E�@��@��@�h@�h@�@`B@O�@/@/@�@V@��@��@�@z�@Z@1@ƨ@ƨ@�F@��@t�@S�@o@��@�^@Ĝ@�P@�@�y@�y@�@ȴ@ȴ@ȴ@ȴ@�+@v�@E�@E�@E�@E�@E�@E�@�T@�T@�-@��@p�@O�@��@�j@I�@�m@ƨ@��@"�@
�!@
~�@
=q@	��@	��@	x�@	G�@	%@Ĝ@A�@1'@1'@b@  @��@�@�P@|�@\)@K�@;d@;d@;d@�@��@��@O�@�@V@�@j@�@ƨ@ƨ@ƨ@��@C�@o@@�H@��@�!@^5@-@��@hs@ �`@ r�?�;d?���?���?��?��?��?���?��?���?�v�?�V?�V?�5??�{?�{?���?���?���?��-?�O�?�/?�V?���?�j?���?�?��H?�?��H?�?�?���?�~�?�^5?��#?���?�b?�l�?�K�?�K�?�K�?�
=?�ff?�$�?��T?�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111�A�$�A�&�A�&�A�&�A�+A�(�A�+A�-A�
=AӸRA�Q�A�33A���A�=qA�  A�O�A�{A�7LA��-A��\A�XA���A���A���A�;dA��A��^A��A��A�dZA�A��A���A�"�A��yA�(�A�ȴA���A�XA��mA���A�9XA��A�jA�$�A���A���A��A�z�A�5?A��A���A�VA�jA��;A�E�A�  A���A���A�ȴA�bNA�XAC�A~�!A~{A}��A|ĜAyx�Au�#Ar��Ao�TAm�Alr�Ak��Ak�Aj9XAi&�Ah��Ah=qAg�
Af�Ad�!Ad5?Ac�hAbĜAbbNAa��Aa7LA`ffA_��A_XA]�-A\Q�A[�AZ�uAYx�AX$�AWXAV^5AU�AU�PAU7LAT{ASS�AR$�AQ�APQ�AOl�AN�DAMS�AK33AI��AI��AI33AH�AHQ�AGt�AG7LAF�/AFM�AE/ADQ�AD-ACƨACt�AC�AB��AB��ABA�AA�AA�TAA�;AA�#AA��AAhsA?��A>�/A=%A;�;A;;dA:�DA:bA9��A9�hA8�A89XA7�A6��A6=qA41A3XA2�A2��A2A�A1\)A0��A/�A/\)A/
=A.ȴA.VA-ƨA-K�A,��A+�;A+?}A*�DA)�mA)�A(�A(A'S�A&��A&bA%|�A%�A$�\A$5?A$JA#�TA#�FA#��A#��A#t�A#C�A#/A#
=A"��A"bNA!�A  A�HA��A �A��A��A�AVA�A��AbNA{A��A�A1A`BA�A�/A�A��A�`AVA(�A|�A��A��A&�A�A5?A��A	�mA	�PA	`BAĜAƨA5?AƨA��AS�A�/A��A
=AȴA��AZA  A�FA�A%A Z@�dZ@�;d@��@�hs@�z�@���@�b@�
=@��!@�=q@���@�"�@�@�A�@�X@�!@�x�@�D@��@�-@��m@��@���@��@�z�@� �@�~�@��;@��T@أ�@�7L@�9X@���@�\)@�$�@���@Ϯ@ϕ�@�C�@�ȴ@ͺ^@�I�@�+@�^5@ɑh@��@�r�@�\)@ģ�@��#@�K�@��@��/@��j@�K�@��#@��@�
=@�x�@�Q�@�1@��
@���@�l�@�C�@�
=@��H@���@�@�&�@��@�z�@�A�@�ƨ@�J@���@��@���@�t�@�ff@��@��9@�
=@��@�V@�9X@���@�S�@��@�5?@�%@��@�1'@��m@��R@�ff@���@�@��j@�o@��R@�n�@�$�@��@��@��-@�7L@�%@��@��@�bN@�  @�t�@�~�@�hs@�Ĝ@���@�@��H@���@��+@�5?@��^@���@�?}@��@�1@���@��@�x�@���@��@�Z@���@��;@��m@�C�@���@���@�{@���@���@�S�@�+@���@���@��`@�  @���@�t�@�K�@�"�@��@���@��!@��+@�E�@�@��7@�p�@�p�@�hs@�X@�O�@�G�@�/@�V@���@��`@��j@��D@�Z@�I�@�1'@�1'@�1'@�b@��@~�@~V@~E�@~$�@~@}�T@}�@}/@|�@|9X@{��@{�@{t�@{dZ@{S�@z�H@y�@yG�@x�`@x��@x�9@x�u@xr�@x �@w�@wK�@v��@v�y@v��@vE�@v5?@v$�@v@u�@u��@u��@u��@u�h@u`B@t�@t9X@s@rJ@p�9@p  @oK�@n�R@m`B@m�@lZ@l1@k�F@k��@kt�@k33@j�@j�!@j^5@i��@i7L@h��@hbN@g|�@g;d@g�@f��@f�R@f�+@fv�@fV@f5?@f$�@f@e�h@e/@d�j@d��@d�D@dz�@c�m@b�H@b�@a&�@a%@`�9@`1'@`b@`b@`b@` �@`b@`b@`b@`b@` �@`b@`b@`  @_�;@_�w@_;d@^ȴ@]�h@\��@\�j@\�j@\�j@\�D@]V@\��@\�@\��@\�/@\�j@\z�@\Z@\9X@[��@Zn�@Y7L@YG�@YX@Z=q@Z-@Zn�@Z^5@Z=q@Y��@Y�^@Y�^@Y�7@Yhs@X��@X�@X��@X��@Xr�@XbN@XbN@Xr�@XA�@X  @W�@Vv�@Up�@Up�@U?}@T��@T��@T9X@S��@S�m@S�
@S�F@S�@S33@S33@S"�@R��@R~�@RM�@Q��@Q&�@P��@PbN@O�w@N�y@N@Mp�@L��@Lj@L�@K��@K�F@KdZ@K33@Ko@JM�@I�@H��@HbN@Hb@G�@G�w@G�P@G\)@G\)@GK�@G�@F��@F��@Fv�@F$�@F{@F@E�@E�T@E�T@E��@E@E�-@EO�@EV@D�/@D�@D��@D�@D�@C��@CdZ@CS�@CC�@B�!@BJ@A��@AX@@�`@@��@@�@@�@@�@@bN@@bN@@ �@@ �@@b@@  @?�@?�;@?�@?|�@?K�@?;d@?�@>��@>ȴ@>��@>��@>�+@>ff@>V@>E�@>$�@=�h@=p�@=V@<�@<�D@<z�@<z�@<z�@<z�@<Z@;�m@;�m@;�m@;��@;S�@;"�@:�H@:�\@:J@9��@9��@9��@9�7@9&�@8�u@7�;@7l�@6�@6��@6�+@6v�@6E�@5�@5��@5��@5@5�@5�@4�@4��@4�@4Z@4�@333@2�@2�!@2��@2�\@2M�@2-@2-@2-@2�@2�@1��@1x�@0�`@0�u@0A�@0b@/��@/�P@/\)@/K�@/;d@.��@.�@.��@.�+@.v�@.ff@.E�@.5?@.E�@.5?@.{@-�h@,��@,��@,��@,I�@,(�@,�@,1@,1@+ƨ@+��@+dZ@*�@*^5@*-@*�@*�@*J@)%@(Ĝ@(r�@(r�@(1'@(b@(  @(  @(  @'�@'�@'�;@'�w@'�P@'l�@'\)@'�@&ff@%�-@%�h@%�h@%�@%`B@%?}@$I�@"M�@!��@!hs@!X@!G�@!G�@ ��@ �`@ �@ r�@  �@�w@|�@|�@�@�y@�y@�y@�@�y@v�@v�@ff@V@$�@{@@{@ff@E�@�-@��@@��@O�@O�@O�@��@z�@�@(�@�
@�F@�F@��@��@��@t�@t�@t�@t�@dZ@C�@"�@�^@��@ �@��@�P@
=@�@��@V@E�@��@��@�h@�h@�@`B@O�@/@/@�@V@��@��@�@z�@Z@1@ƨ@ƨ@�F@��@t�@S�@o@��@�^@Ĝ@�P@�@�y@�y@�@ȴ@ȴ@ȴ@ȴ@�+@v�@E�@E�@E�@E�@E�@E�@�T@�T@�-@��@p�@O�@��@�j@I�@�m@ƨ@��@"�@
�!@
~�@
=q@	��@	��@	x�@	G�@	%@Ĝ@A�@1'@1'@b@  @��@�@�P@|�@\)@K�@;d@;d@;d@�@��@��@O�@�@V@�@j@�@ƨ@ƨ@ƨ@��@C�@o@@�H@��@�!@^5@-@��@hs@ �`@ r�?�;d?���?���?��?��?��?���?��?���?�v�?�V?�V?�5??�{?�{?���?���?���?��-?�O�?�/?�V?���?�j?���?�?��H?�?��H?�?�?���?�~�?�^5?��#?���?�b?�l�?�K�?�K�?�K�?�
=?�ff?�$�?��T?�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111�BBBBBBBB��B��B�qB��B�7B�7B�Bs�BH�BD�BA�B;dB&�B%B��B�B��B��B��B��B��B��B��B��B��B��B�1Bt�Bq�Bn�Bl�BhsBdZBe`Be`B^5BW
BgmBq�Bo�B^5BT�BQ�BJ�B>wB1'B)�B$�B�B�B\B
��B
�fB
��B
�wB
�3B
�B
��B
��B
��B
~�B
bNB
I�B
5?B
#�B
�B
�B
oB
JB
B
B	��B	��B	�B	�ZB	�BB	�)B	�B	��B	��B	��B	ȴB	ŢB	��B	�RB	�'B	�B	��B	��B	��B	�uB	�VB	�=B	�1B	�B	|�B	w�B	p�B	jB	e`B	`BB	ZB	R�B	J�B	E�B	C�B	A�B	?}B	<jB	9XB	7LB	5?B	2-B	.B	+B	)�B	'�B	%�B	$�B	#�B	"�B	!�B	 �B	�B	�B	�B	�B	�B	�B	\B	+B	B��B��B��B��B��B�B�B�B�B�fB�;B�)B�B�B�
B��B��B��B��B��BɺBǮBŢBÖB��B�wB�dB�XB�FB�3B�'B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�uB�JB�7B�+B�%B�B�B}�Bz�Bv�Bt�Br�Bq�Bo�Bl�BjBhsBgmBffBdZBbNB`BB^5B]/B[#BXBVBS�BQ�BP�BM�BJ�BI�BH�BF�BB�B@�B>wB>wB=qB;dB9XB7LB7LB7LB6FB5?B49B33B2-B0!B/B.B-B,B(�B&�B%�B$�B#�B"�B!�B�B�B�B�B�B�B�B�B�B�B{BuBuBuBhBbB\B\BPBVBVBVBPBPBPBVBPBPBJBJBJBJBPBPBPBJBDBJBVBhB{B{BuB�B�B�B�B�B�B�B �B!�B!�B"�B"�B"�B"�B#�B+B/B2-B2-B2-B5?B6FB:^B;dB;dB<jB=qB?}B@�B?}B@�BF�BH�BH�BJ�BM�BN�BM�BM�BM�BL�BL�BN�BR�B[#BcTBdZBe`BffBffBgmBjBn�Bo�Bp�Bq�Bq�Br�Bt�By�B~�B�B�+B�DB�DB�JB�PB�VB�bB�bB�oB��B��B��B��B��B�B�B�!B�9B�?B�9B�XB�qB�qB��BƨB��B��B��B��B�
B�)B�TB�fB�mB�yB�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B	B	B	B	B	B	%B	1B	VB	hB	hB	oB	oB	uB	{B	�B	�B	�B	�B	�B	�B	�B	�B	�B	!�B	$�B	%�B	&�B	&�B	'�B	'�B	)�B	+B	-B	.B	/B	0!B	1'B	1'B	2-B	2-B	33B	33B	49B	49B	49B	49B	6FB	8RB	<jB	?}B	B�B	C�B	F�B	H�B	L�B	N�B	Q�B	S�B	T�B	W
B	XB	YB	[#B	[#B	\)B	]/B	_;B	`BB	bNB	gmB	hsB	iyB	iyB	jB	k�B	k�B	l�B	l�B	m�B	n�B	p�B	q�B	t�B	t�B	u�B	v�B	}�B	�+B	�1B	�JB	�JB	�PB	�\B	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�-B	�?B	�LB	�XB	�XB	�^B	�^B	�^B	�^B	�^B	�^B	�^B	�dB	�qB	��B	ÖB	ĜB	ĜB	ĜB	ĜB	ŢB	ŢB	ŢB	ŢB	ƨB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�
B	�B	�B	�B	�B	�)B	�/B	�5B	�BB	�NB	�ZB	�ZB	�`B	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
+B
1B
1B

=B
DB
DB
JB
JB
JB
JB
JB
PB
PB
PB
PB
PB
PB
PB
PB
VB
VB
VB
VB
VB
\B
\B
\B
\B
\B
\B
\B
bB
hB
hB
oB
uB
uB
uB
uB
uB
uB
{B
�B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
 �B
 �B
 �B
!�B
"�B
"�B
"�B
"�B
#�B
#�B
%�B
%�B
&�B
&�B
&�B
&�B
'�B
&�B
'�B
'�B
'�B
'�B
(�B
)�B
)�B
+B
,B
,B
-B
-B
-B
-B
.B
.B
/B
/B
/B
/B
0!B
0!B
0!B
0!B
0!B
1'B
2-B
2-B
33B
33B
49B
49B
49B
49B
49B
5?B
5?B
5?B
7LB
7LB
7LB
7LB
7LB
9XB
9XB
:^B
:^B
;dB
;dB
;dB
;dB
;dB
;dB
;dB
;dB
<jB
<jB
<jB
=qB
=qB
?}B
A�B
A�B
A�B
A�B
A�B
@�B
A�B
E�B
F�B
G�B
G�B
G�B
G�B
H�B
G�B
H�B
H�B
H�B
I�B
I�B
I�B
J�B
J�B
J�B
J�B
J�B
J�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
L�B
L�B
L�B
K�B
L�B
L�B
L�B
L�B
M�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
M�B
O�B
Q�B
Q�B
R�B
R�B
S�B
S�B
S�B
T�B
T�B
VB
VB
VB
VB
VB
VB
VB
VB
VB
VB
VB
W
B
W
B
W
B
W
B
W
B
XB
XB
XB
XB
XB
YB
YB
YB
YB
ZB
\)B
]/B
^5B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
`BB
_;B
_;B
`BB
_;B
`BB
`BB
`BB
`BB
aHB
aHB
aHB
aHB
bNB
cTB
cTB
cTB
cTB
dZB
dZB
e`B
e`B
e`B
ffB
ffB
ffB
gmB
hsB
hsB
hsB
hsB
hsB
hsB
hsB
iyB
iyB
iyB
iyB
iyB
iyB
iyB
iyB
iyB
k�B
l�B
l�B
l�B
l�B
m�B
m�B
n�B
n�B
n�B
n�B
o�B
o�B
o�B
o�B
o�B
o�B
p�B
p�B
p�B
q�B
r�B
r�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
v�B
v�B
v�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
x�B
x�B
x�B
x�B
y�B
z�B
z�B
z�B
z�B
z�B
{�B
{�B
{�B
|�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111�BBBBBBªB��B��B�BǮB�"B�B��B�rBcBN�BJ�BF�B?B-)BB�FB�B��B��B�eB��B�B�yB�B��B��B�kB�BvzBr�BoOBm�Bk�Bf�Bf�Bf2B_!BW�Bh>BsBraB_�BU�BS[BL~B@�B2�B+QB&2B�BB@B
��B
�yB
�:B
� B
�B
� B
��B
��B
��B
�GB
f2B
MB
7�B
%B
�B
sB
�B
�B
�B
B	��B	��B	�B	�,B	�-B	�B	خB	յB	��B	��B	ɠB	��B	B	�B	�GB	�]B	�mB	�HB	��B	��B	�B	��B	��B	�mB	~B	yXB	q�B	k�B	f�B	a�B	[�B	UgB	L0B	F?B	DMB	B'B	@OB	=VB	9�B	7�B	6+B	3�B	/ B	+QB	*�B	(sB	&fB	%`B	$ZB	#TB	"4B	 �B	�B	 B	 'B	�B	�B	
B	�B	�B	�B��B�B�*B��B��B��B�B�B��B��B�BB��BںB��B�EB��B�BΊB�JB�DB�rBȀB�tBĶB��B�cB�jB�DB�LB�B��B�!B��B�"B��B��B��B�tB� B�B�B��B��B�B�'B��B�!B�jB��B��B�$B��B��B�B�+B�MB�uBcB|�BxBu�BsMBraBp�Bm�BkQBh�Bg�BgBe�BcTB`�B^�B^OB\]BYKBV�BT�BR�BR BO�BK^BJrBI�BH1BDgBA B>�B?B>wB<�B:xB7�B7�B7�B6�B5�B4�B4B3B0�B/�B.�B-�B-)B+B'�B&�B%FB$ZB#�B#nB �B B~B#BBEB_B�B�BBB,B�B�B�BB�B�B(BB�B�B<B<B�BpB�B�B6B6BB�B�B�BB�BPB"BB�B�B�B�B�BsB�B�B~B 'B B!B!�B!�B#B# B#:B#nB$�B+QB/�B2�B2�B3hB5�B72B:�B<B<6B="B>BB@�BA;B@OBAUBGBIBI7BKxBN�BOBBNVBN<BN�BM6BM6BOBBS�B\)Bc�Bd�Be�Bf�Bf�Bg�Bj�Bn�BpBp�Bq�BrBshBu�Bz�B�B��B��B�xB�xB��B��B��B��B��B�B�B��B�jB�@B�sB�kB�]B��B�nB��B��B��B��B�(B��B�EB�0B�HB�hBөB��B��B�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B�B��B��B�B�B�0B�B�<B�.B	AB	-B	-B	MB	MB	tB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	!B	!�B	$�B	%�B	'B	'B	(
B	($B	*0B	+QB	-)B	./B	/OB	0UB	1[B	1AB	2aB	2aB	3hB	3hB	4nB	4TB	4nB	4�B	6�B	8�B	<�B	@ B	B�B	C�B	F�B	IB	MB	OBB	R:B	TB	UB	W$B	XEB	YKB	[WB	[WB	\]B	]dB	_�B	`�B	b�B	g�B	h�B	i�B	i�B	j�B	k�B	k�B	l�B	l�B	m�B	n�B	p�B	q�B	t�B	t�B	u�B	wLB	~wB	�zB	��B	�dB	�~B	��B	�vB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�2B	�LB	�B	�*B	�B	�B	�)B	��B	�-B	�?B	�fB	��B	��B	�xB	�xB	�xB	��B	��B	��B	�^B	�B	�VB	��B	ÖB	ĶB	ĶB	��B	ĶB	ŢB	żB	��B	��B	��B	ǮB	��B	��B	��B	��B	��B	��B	�B	�B	�6B	�B	��B	�B	�B	�B	� B	�B	�B	�B	�$B	�?B	�+B	�B	�7B	�QB	�CB	�IB	ޞB	�vB	�B	�B	�B	�B	��B	��B	�B	��B	��B	�B	�B	�B	�B	��B	��B	�B	��B	��B	��B	�B	�B	��B	��B	��B	��B	��B	�B	�B	�B	�B	��B	��B	�B	��B	��B	�B	�B	�B	�HB
 B
 B
;B
;B
'B
UB
aB
3B
SB
SB
SB
_B
KB
fB

XB
xB
^B
JB
dB
dB
JB
~B
PB
jB
PB
jB
�B
jB
jB
jB
pB
pB
pB
pB
pB
\B
vB
�B
�B
vB
�B
�B
�B
�B
�B
�B
�B
uB
�B
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
�B
�B
�B
�B
�B
�B
�B
 �B
 �B
 �B
 �B
!�B
"�B
"�B
"�B
"�B
$B
$&B
&B
&B
'B
'B
'B
'B
'�B
&�B
'�B
($B
(
B
(>B
)*B
*B
*0B
+6B
,"B
,"B
-)B
-B
-CB
-)B
.IB
.IB
/B
/5B
/B
/5B
0!B
0;B
0UB
0;B
0UB
1vB
2aB
2aB
3hB
3MB
4TB
49B
4TB
4TB
4TB
5tB
5�B
5�B
7�B
7LB
7�B
7�B
7�B
9rB
9�B
:^B
:�B
;B
;dB
;B
;dB
;dB
;B
;B
;�B
<�B
<�B
<�B
=�B
=�B
?�B
A�B
A�B
A�B
A�B
A�B
AB
B'B
E�B
F�B
G�B
G�B
G�B
G�B
H�B
G�B
H�B
H�B
H�B
I�B
I�B
I�B
J�B
J�B
J�B
J�B
J�B
J�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
L�B
L�B
L�B
K�B
L�B
MB
MB
MB
M�B
N�B
OB
OB
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
O(B
NVB
PHB
R B
R B
SB
S@B
TB
TB
T,B
UB
UMB
VB
VB
VB
VB
VB
VB
V9B
VB
VB
VB
VB
W$B
W$B
W?B
W?B
W?B
X+B
X+B
XB
X+B
XEB
Y1B
YKB
YKB
YB
Z�B
\�B
]dB
^jB
_VB
_;B
_;B
_VB
_;B
_;B
_VB
_VB
_VB
_;B
`BB
_;B
_VB
`BB
_VB
`BB
`\B
`\B
`vB
a|B
a|B
abB
a|B
b�B
c�B
c�B
c�B
c�B
dtB
dtB
e�B
e�B
e�B
f�B
f�B
f�B
g�B
hsB
h�B
h�B
h�B
h�B
h�B
h�B
iyB
i�B
i�B
iyB
iyB
iyB
i�B
i�B
i�B
k�B
l�B
l�B
l�B
l�B
m�B
m�B
n�B
n�B
n�B
n�B
o�B
o�B
o�B
o�B
o�B
o�B
p�B
p�B
p�B
q�B
r�B
r�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
v�B
v�B
v�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
xB
y	B
x�B
y	B
y$B
zB
z�B
z�B
z�B
z�B
{B
{�B
{�B
|B
|�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111�@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<�a�<-��<#�
<#�
<:�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.08(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201708020034472017080200344720170802003447202211182131112022111821311120221118213111201804031936362018040319363620180403193636  JA  ARFMdecpA19c                                                                20170723003508  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20170722153717  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20170722153720  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20170722153721  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20170722153722  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20170722153722  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20170722153722  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20170722153722  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20170722153722  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20170722153722                      G�O�G�O�G�O�                JA  ARUP                                                                        20170722161500                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20170722153230  CV  JULD            G�O�G�O�F��                JM  ARCAJMQC2.0                                                                 20170801153447  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20170801153447  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180403103636  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200114171526                      G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20221118123111  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20221123111507                      G�O�G�O�G�O�                