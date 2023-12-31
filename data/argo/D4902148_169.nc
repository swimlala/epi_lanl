CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       ~2019-04-03T15:36:42Z creation;2019-04-03T15:36:45Z conversion to V3.1;2019-12-18T07:15:59Z update;2022-11-21T05:29:09Z update;     
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
_FillValue                 �  ]�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ap   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  q(   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  u   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �x   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �h   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  π   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �8   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �H   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �L   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �P   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �T   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �X   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20190403153642  20221123111508  4902148 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I1_0397_169                     2C  Dd_NAVIS_A                         0397                            ARGO 011514                     863 @س��~K 1   @س�s�� @<����t�d_��F1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�3D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D���D�� 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111�
@���@�p�@�p�A�RA>�RA^�RA~�RA�\)A�\)A�\)A�\)A�\)A�\)A�\)A�\)B�B�B�B�B'�B/�B7�B?�BG�BO�BW�B_�Bg�Bo�Bw�B�B��
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
��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��D z�D ��D!z�D!��D"z�D"��D#z�D#��D$z�D$��D%z�D%��D&z�D&��D'z�D'��D(z�D(��D)z�D)��D*z�D*��D+z�D+��D,z�D,��D-z�D-��D.z�D.��D/z�D/��D0z�D0��D1z�D1��D2z�D2��D3z�D3��D4z�D4��D5z�D5��D6z�D6��D7z�D7��D8z�D8��D9z�D9��D:z�D:��D;z�D;��D<z�D<��D=z�D=��D>z�D>��D?z�D?��D@z�D@��DAz�DA��DBz�DB��DCz�DC��DDz�DD��DEz�DE��DFz�DF��DGz�DG��DHz�DH��DIz�DI��DJz�DJ��DKz�DK��DLz�DL��DMz�DM��DNz�DN��DOz�DO��DPz�DP��DQz�DQ��DRz�DR��DSz�DS��DTz�DT��DUz�DU��DVz�DV��DWz�DW��DXz�DX��DYz�DY��DZz�DZ��D[z�D[��D\z�D\��D]z�D]��D^z�D^��D_z�D_��D`z�D`��Daz�Da��Dbz�Db��Dcz�Dc��Ddz�Dd��Dez�De��Dfz�Df��Dgz�Dg��Dhz�Dh��Diz�Di��Djz�Dj��Dkz�Dk��Dlz�Dl��Dmz�Dm��Dnz�Dn��Doz�Do��Dpz�Dp��Dqz�Dq��Drz�Dr��Dsz�Ds��Dtz�Dt��Duz�Du��Dvz�Dv��Dwz�Dw��Dxz�Dx��Dyz�Dy��Dzz�Dz��D{z�D{��D|z�D|��D}z�D}��D~z�D~��Dz�D��D�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD½qD��qD�=qD�}qDýqD��qD�=qD�}qDĽqD��qD�=qD�}qDŽqD��qD�=qD�}qDƽqD��qD�=qD�}qDǽqD��qD�=qD�}qDȽqD��qD�=qD�}qDɽqD��qD�=qD�}qDʽqD��qD�=qD�}qD˽qD��qD�=qD�}qD̽qD��qD�=qD�}qDͽqD��qD�=qD�}qDνqD��qD�=qD�}qDϽqD��qD�=qD�}qDнqD��qD�=qD�}qDѽqD��qD�=qD�}qDҽqD��qD�=qD�}qDӽqD��qD�=qD�}qDԽqD��qD�=qD�}qDսqD��qD�=qD�}qDֽqD��qD�=qD�}qD׽qD��qD�=qD�}qDؽqD��qD�=qD�}qDٽqD��qD�=qD�}qDڽqD��qD�=qD�}qD۽qD��qD�=qD�}qDܽqD��qD�=qD�}qDݽqD��qD�=qD�}qD޽qD��qD�=qD�}qD߽qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD� �D�=qD�}qD��qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�z=D�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD��
D��q1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111�9A�bNA�ffA�hsA�ZA�/A�oA���A��A��`A��;A��#A���A���A���A�ȴA�ƨA�ĜA�ĜA�ĜA�A��jA��RA��RA��^A��^A��^A��RA��FA��FA��RA��FA��FA��9A��9A��9A��9A��9A��FA��FA��RA��RA��RA��^A��RA��9A��9A��9A��9A��9A��9A��-A��-A��-A��!A��!A��!A��A���A���A���A���A���A���A��\A�|�A�r�A�p�A�dZA�1A���A���A���A�ffA��A���A���A�(�A�G�A��#A��A�9XA���A�v�A�?}A�|�A��A�
=A�A�A��A��A�VA�%A���A�;dA�hsA�&�A���A~�DA}
=A|Az�AxbNAv��At��ArbNAn(�Al�RAkoAi��Ah�9Af�yAd�yAc��Ab�Aa7LA_�A]�;A]p�A\�A\9XA[�mA[�AYAW7LAU��AUG�ATE�AR�yARE�AQ�TAQ�hAQdZAP�/AP{AO|�AO
=ANv�AM��ALr�AK�^AJn�AH�DAH-AG�AF��AF�AF�+AF{AD�AB��ABVAA�AA?}A>�A=�A=G�A=VA<n�A:�9A8(�A6��A6JA4��A3
=A2�DA2jA2-A1�A0VA/�#A/oA.ffA-��A-�FA-��A,�uA+O�A*=qA)7LA(�jA(-A(1A'�A'�;A'�-A'hsA&�jA&�+A&  A$�A#��A!��A �/A ��A bNA   Al�Ar�A{AXA�DA�TA�wA�A�AA�AXAoAȴAO�Ar�A��Ax�A`BA�A�9An�A-A��AA�7A&�A��A��A�AS�AM�A�PAbNA�^A
�HA
�A	�PAx�AjA��A�A��AM�A�PA�A�9Av�A�A �RA E�@��@�33@��+@���@��j@�K�@�/@���@�I�@�@��@�o@�=q@�p�@�7L@�I�@���@��@��@�\@�-@�b@�;d@�ff@ᙚ@�I�@�V@�dZ@�ȴ@��T@�7L@��@��@�;d@և+@�n�@���@���@�A�@�9X@�1@��;@�o@�5?@�?}@�I�@Ϯ@�+@̓u@�@���@�1@�I�@\@��@��@�j@��@�33@�@��R@�{@��h@���@�Z@�1'@��@�\)@��R@�$�@��h@��@�z�@���@�^5@�p�@��u@��H@�{@��@���@�r�@�1'@�t�@�E�@�x�@�9X@��@��R@�-@��^@��/@�E�@���@��@�
=@���@��\@��\@�~�@�n�@�V@���@�x�@�&�@���@�Z@�l�@��R@���@��@��!@�5?@��9@� �@���@�|�@�+@��y@�J@��u@�(�@��w@�K�@�o@��@���@�ȴ@���@��R@�V@�5?@���@���@�r�@�A�@��@���@��;@��w@���@�dZ@�C�@���@��@��@�&�@��/@�Q�@��w@��y@���@�-@��^@��@�X@���@��;@�|�@�K�@���@�v�@���@���@�@�@��-@��-@���@��7@�&�@��`@��9@�Z@�1@���@�"�@���@��!@�~�@�@���@���@���@�O�@���@� �@�b@�b@�b@�1@�  @�  @�  @�  @�  @���@��@��;@��;@��;@��;@��
@��
@�ƨ@�ƨ@���@��@�l�@�dZ@�33@�
=@���@��@�=q@���@��@��@��D@�9X@�(�@�1@l�@~ȴ@~�+@~V@}��@|�j@{�@z�@z~�@z�@y��@y��@y�7@y7L@x��@x��@xQ�@x �@x  @w�@w�@w�;@w��@w�w@w��@wl�@v�R@u��@t��@t�D@tj@t9X@t1@s��@r~�@r=q@r-@q��@q&�@p1'@p  @o�;@o�@o�@n��@nff@nff@nff@nV@n5?@n{@m��@m`B@l��@lz�@kƨ@k�@kt�@kdZ@j�!@ihs@h��@h�`@h�@g��@f��@f{@e�T@e�h@eO�@d�/@d�@dZ@d9X@d1@c��@cdZ@c33@b�H@bM�@a��@`��@`�@`  @_��@_�w@_�w@_�w@_�w@_�w@_��@_��@_\)@_+@^ȴ@^ȴ@^��@^�+@^v�@^v�@^5?@]`B@]`B@]O�@]?}@]/@]�@\Z@[t�@[C�@[33@Z�H@YX@Y%@XbN@W�@W|�@Wl�@Wl�@W\)@WK�@W+@W+@W+@Vv�@V@U�h@Up�@U`B@UO�@UV@T9X@S��@S�
@S�F@S��@S��@S��@St�@SdZ@SS�@SC�@S33@S@R��@R^5@R=q@Q��@Q�7@Qhs@QX@QX@P�`@P��@P�@Pr�@Pr�@PQ�@PQ�@PQ�@PQ�@P1'@O�;@O��@O�w@O�@Nȴ@M�@M��@MV@L�j@L��@L�D@Lj@LI�@L9X@K�
@Kƨ@K�F@K��@KdZ@J�H@J��@J^5@J�@I��@Ihs@I&�@H��@H��@H��@Hr�@G�@G��@GK�@Fȴ@Fv�@F@E��@E�h@EO�@E�@DZ@D�@C��@C�F@CdZ@CC�@C33@C33@B�@B�!@B�!@B��@B^5@A�@A�^@A��@AX@@�`@@Ĝ@@r�@?�P@>�y@>��@>��@>�+@>�+@>�+@>v�@>v�@>ff@=�@<9X@<�@<1@;��@;�
@;33@:�@:~�@:-@9��@8��@8r�@81'@8A�@81'@8 �@7�;@7\)@7
=@6��@6�y@6ȴ@6v�@6V@6V@6V@6@5�-@5p�@5`B@5/@5V@4�@4�j@4z�@4j@4I�@4�@3ƨ@333@333@2�@2=q@2J@1�@1�7@17L@0��@0�@0Q�@0b@0  @/��@/��@/�@/l�@/
=@.ff@-�@-`B@-`B@-/@,�@,�j@,�j@,�j@,�@,��@,��@,��@,��@,��@,��@,��@,��@,�D@,j@+��@*��@*M�@*�@)��@)�7@)7L@(�`@(��@(�u@(r�@(1'@( �@'�;@'�w@'�@'��@'|�@'K�@&��@%�T@%@%��@%�h@%`B@%/@%�@$��@$�@$��@$�j@$�@$�D@$(�@#ƨ@#t�@#33@"�H@"�!@"~�@"M�@"=q@"=q@"=q@"-@!�@!�@!%@!%@ ��@ �`@ ��@ ��@ ��@ �9@ �@ �@ r�@ A�@  �@�@��@\)@�@
=@��@��@��@��@��@��@��@�+@V@5?@@@O�@�/@�@z�@9X@1@ƨ@��@t�@t�@t�@S�@33@@��@~�@=q@J@��@�@Q�@b@��@�@|�@\)@;d@�@
=@�y@ȴ@��@ff@{@@�T@�-@p�@/@��@�/@�@��@�D@j@j@9X@9X@�@�F@dZ@33@�H@��@�\@=q@�@��@��@��@�@�^@��@��@�7@�7@x�@G�@&�@&�@%@��@��@�9@Q�@  @|�@K�@;d@+@�@
=@ȴ@V@{@�@��@��@�-@�h@�@�@O�@�/@j@Z@(�@C�@@
�@
�!@
^5@
M�@
J@	�^@	��@	��@	��@	�7@	�7@	x�@	x�@	hs@	X@	7L@��@�u@A�@b@b@b@b@b@  @  @  @  @�@�@��@l�@;d@+@
=@��@��@�y@�y@�@�R@��@��@��@5?@@p�@?}@/@�@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111�9A�bNA�ffA�hsA�ZA�/A�oA���A��A��`A��;A��#A���A���A���A�ȴA�ƨA�ĜA�ĜA�ĜA�A��jA��RA��RA��^A��^A��^A��RA��FA��FA��RA��FA��FA��9A��9A��9A��9A��9A��FA��FA��RA��RA��RA��^A��RA��9A��9A��9A��9A��9A��9A��-A��-A��-A��!A��!A��!A��A���A���A���A���A���A���A��\A�|�A�r�A�p�A�dZA�1A���A���A���A�ffA��A���A���A�(�A�G�A��#A��A�9XA���A�v�A�?}A�|�A��A�
=A�A�A��A��A�VA�%A���A�;dA�hsA�&�A���A~�DA}
=A|Az�AxbNAv��At��ArbNAn(�Al�RAkoAi��Ah�9Af�yAd�yAc��Ab�Aa7LA_�A]�;A]p�A\�A\9XA[�mA[�AYAW7LAU��AUG�ATE�AR�yARE�AQ�TAQ�hAQdZAP�/AP{AO|�AO
=ANv�AM��ALr�AK�^AJn�AH�DAH-AG�AF��AF�AF�+AF{AD�AB��ABVAA�AA?}A>�A=�A=G�A=VA<n�A:�9A8(�A6��A6JA4��A3
=A2�DA2jA2-A1�A0VA/�#A/oA.ffA-��A-�FA-��A,�uA+O�A*=qA)7LA(�jA(-A(1A'�A'�;A'�-A'hsA&�jA&�+A&  A$�A#��A!��A �/A ��A bNA   Al�Ar�A{AXA�DA�TA�wA�A�AA�AXAoAȴAO�Ar�A��Ax�A`BA�A�9An�A-A��AA�7A&�A��A��A�AS�AM�A�PAbNA�^A
�HA
�A	�PAx�AjA��A�A��AM�A�PA�A�9Av�A�A �RA E�@��@�33@��+@���@��j@�K�@�/@���@�I�@�@��@�o@�=q@�p�@�7L@�I�@���@��@��@�\@�-@�b@�;d@�ff@ᙚ@�I�@�V@�dZ@�ȴ@��T@�7L@��@��@�;d@և+@�n�@���@���@�A�@�9X@�1@��;@�o@�5?@�?}@�I�@Ϯ@�+@̓u@�@���@�1@�I�@\@��@��@�j@��@�33@�@��R@�{@��h@���@�Z@�1'@��@�\)@��R@�$�@��h@��@�z�@���@�^5@�p�@��u@��H@�{@��@���@�r�@�1'@�t�@�E�@�x�@�9X@��@��R@�-@��^@��/@�E�@���@��@�
=@���@��\@��\@�~�@�n�@�V@���@�x�@�&�@���@�Z@�l�@��R@���@��@��!@�5?@��9@� �@���@�|�@�+@��y@�J@��u@�(�@��w@�K�@�o@��@���@�ȴ@���@��R@�V@�5?@���@���@�r�@�A�@��@���@��;@��w@���@�dZ@�C�@���@��@��@�&�@��/@�Q�@��w@��y@���@�-@��^@��@�X@���@��;@�|�@�K�@���@�v�@���@���@�@�@��-@��-@���@��7@�&�@��`@��9@�Z@�1@���@�"�@���@��!@�~�@�@���@���@���@�O�@���@� �@�b@�b@�b@�1@�  @�  @�  @�  @�  @���@��@��;@��;@��;@��;@��
@��
@�ƨ@�ƨ@���@��@�l�@�dZ@�33@�
=@���@��@�=q@���@��@��@��D@�9X@�(�@�1@l�@~ȴ@~�+@~V@}��@|�j@{�@z�@z~�@z�@y��@y��@y�7@y7L@x��@x��@xQ�@x �@x  @w�@w�@w�;@w��@w�w@w��@wl�@v�R@u��@t��@t�D@tj@t9X@t1@s��@r~�@r=q@r-@q��@q&�@p1'@p  @o�;@o�@o�@n��@nff@nff@nff@nV@n5?@n{@m��@m`B@l��@lz�@kƨ@k�@kt�@kdZ@j�!@ihs@h��@h�`@h�@g��@f��@f{@e�T@e�h@eO�@d�/@d�@dZ@d9X@d1@c��@cdZ@c33@b�H@bM�@a��@`��@`�@`  @_��@_�w@_�w@_�w@_�w@_�w@_��@_��@_\)@_+@^ȴ@^ȴ@^��@^�+@^v�@^v�@^5?@]`B@]`B@]O�@]?}@]/@]�@\Z@[t�@[C�@[33@Z�H@YX@Y%@XbN@W�@W|�@Wl�@Wl�@W\)@WK�@W+@W+@W+@Vv�@V@U�h@Up�@U`B@UO�@UV@T9X@S��@S�
@S�F@S��@S��@S��@St�@SdZ@SS�@SC�@S33@S@R��@R^5@R=q@Q��@Q�7@Qhs@QX@QX@P�`@P��@P�@Pr�@Pr�@PQ�@PQ�@PQ�@PQ�@P1'@O�;@O��@O�w@O�@Nȴ@M�@M��@MV@L�j@L��@L�D@Lj@LI�@L9X@K�
@Kƨ@K�F@K��@KdZ@J�H@J��@J^5@J�@I��@Ihs@I&�@H��@H��@H��@Hr�@G�@G��@GK�@Fȴ@Fv�@F@E��@E�h@EO�@E�@DZ@D�@C��@C�F@CdZ@CC�@C33@C33@B�@B�!@B�!@B��@B^5@A�@A�^@A��@AX@@�`@@Ĝ@@r�@?�P@>�y@>��@>��@>�+@>�+@>�+@>v�@>v�@>ff@=�@<9X@<�@<1@;��@;�
@;33@:�@:~�@:-@9��@8��@8r�@81'@8A�@81'@8 �@7�;@7\)@7
=@6��@6�y@6ȴ@6v�@6V@6V@6V@6@5�-@5p�@5`B@5/@5V@4�@4�j@4z�@4j@4I�@4�@3ƨ@333@333@2�@2=q@2J@1�@1�7@17L@0��@0�@0Q�@0b@0  @/��@/��@/�@/l�@/
=@.ff@-�@-`B@-`B@-/@,�@,�j@,�j@,�j@,�@,��@,��@,��@,��@,��@,��@,��@,��@,�D@,j@+��@*��@*M�@*�@)��@)�7@)7L@(�`@(��@(�u@(r�@(1'@( �@'�;@'�w@'�@'��@'|�@'K�@&��@%�T@%@%��@%�h@%`B@%/@%�@$��@$�@$��@$�j@$�@$�D@$(�@#ƨ@#t�@#33@"�H@"�!@"~�@"M�@"=q@"=q@"=q@"-@!�@!�@!%@!%@ ��@ �`@ ��@ ��@ ��@ �9@ �@ �@ r�@ A�@  �@�@��@\)@�@
=@��@��@��@��@��@��@��@�+@V@5?@@@O�@�/@�@z�@9X@1@ƨ@��@t�@t�@t�@S�@33@@��@~�@=q@J@��@�@Q�@b@��@�@|�@\)@;d@�@
=@�y@ȴ@��@ff@{@@�T@�-@p�@/@��@�/@�@��@�D@j@j@9X@9X@�@�F@dZ@33@�H@��@�\@=q@�@��@��@��@�@�^@��@��@�7@�7@x�@G�@&�@&�@%@��@��@�9@Q�@  @|�@K�@;d@+@�@
=@ȴ@V@{@�@��@��@�-@�h@�@�@O�@�/@j@Z@(�@C�@@
�@
�!@
^5@
M�@
J@	�^@	��@	��@	��@	�7@	�7@	x�@	x�@	hs@	X@	7L@��@�u@A�@b@b@b@b@b@  @  @  @  @�@�@��@l�@;d@+@
=@��@��@�y@�y@�@�R@��@��@��@5?@@p�@?}@/@�@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�PB�BiyBW
BH�B<jB<jB5?B(�B�BJB��B�sB�B�XB��B�\B�Bo�B^5BM�B0!B
=B
��B
�B
�TB
��B
��B
�PB
u�B
gmB
]/B
P�B
>wB
0!B
 �B
\B	��B	�yB	�)B	��B	ɺB	�wB	�-B	�B	��B	��B	�{B	�DB	�7B	�+B	�B	� B	|�B	w�B	o�B	gmB	e`B	`BB	]/B	]/B	\)B	[#B	ZB	YB	W
B	S�B	Q�B	O�B	K�B	F�B	C�B	=qB	9XB	8RB	49B	1'B	0!B	-B	)�B	"�B	�B	oB	VB	%B��B�B�B�B�mB�#B��BƨBĜBB�jB�^B�dB�XB�9B�B�B��B��B��B��B��B��B�{B�hB�VB�PB�DB�=B�=B�7B�7B�+B�+B�%B�B� B{�Bx�Bv�Bu�Bt�Bt�Br�Bq�Bo�Bn�Bk�BjBiyBiyBgmBdZBcTBbNBaHB_;BZBT�BQ�BQ�BQ�BP�BO�BN�BM�BM�BL�BK�BJ�BI�BH�BF�BD�BB�B@�B=qB;dB9XB6FB49B0!B/B.B,B,B)�B(�B'�B&�B%�B$�B"�B!�B!�B �B�B�B�B�B�B{BoBbB\BVBVBVBPBJBDB
=B
=B
=B	7B	7B	7B1B+B%B+B+B+B+B+B+B%B1B1B1B1B	7B
=B
=B
=B
=BDBoB�B"�B"�B!�B �B �B"�B$�B(�B)�B+B+B+B,B-B-B-B-B.B.B.B.B.B/B1'B2-B49B7LB:^B<jB?}B@�BA�BE�BF�BH�BI�BI�BI�BK�BM�BO�BQ�BVBW
BYBZB\)BffBjBk�Bq�Bs�Bs�Bs�Bs�Bs�Bt�Bv�Bx�Bz�B{�B{�B� B�+B�DB�VB�hB�{B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�!B�!B�!B�3B�9B�FB�^B�qBBŢBǮBɺB��B��B��B��B��B�B�#B�/B�5B�HB�`B�yB�B�B�B��B��B��B	  B	B	B	B	+B		7B	
=B	DB	DB	JB	JB	JB	JB	PB	VB	\B	bB	hB	uB	�B	�B	�B	�B	�B	�B	�B	!�B	$�B	'�B	,B	-B	-B	-B	-B	-B	.B	.B	.B	.B	.B	.B	.B	/B	/B	/B	/B	/B	/B	/B	0!B	1'B	1'B	1'B	2-B	33B	33B	49B	8RB	9XB	<jB	?}B	C�B	E�B	F�B	F�B	I�B	K�B	L�B	L�B	N�B	P�B	S�B	VB	XB	ZB	ZB	[#B	\)B	]/B	^5B	_;B	`BB	aHB	bNB	bNB	cTB	cTB	cTB	dZB	dZB	dZB	e`B	iyB	m�B	o�B	o�B	o�B	o�B	p�B	t�B	v�B	w�B	y�B	z�B	}�B	~�B	~�B	� B	�B	�B	�B	�B	�B	�B	�B	�%B	�%B	�1B	�=B	�JB	�\B	�bB	�bB	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�-B	�?B	�FB	�RB	�XB	�XB	�XB	�XB	�XB	�XB	�^B	�^B	�^B	�dB	�qB	�qB	�wB	�wB	�wB	�wB	�}B	B	B	ÖB	ÖB	ÖB	ÖB	ƨB	ɺB	��B	ɺB	��B	��B	��B	��B	�B	�
B	�
B	�
B	�
B	�
B	�B	�B	�B	�B	�#B	�/B	�/B	�/B	�5B	�5B	�HB	�NB	�NB	�TB	�TB	�TB	�TB	�ZB	�ZB	�ZB	�ZB	�`B	�`B	�fB	�mB	�mB	�sB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
%B
+B
1B
	7B

=B

=B
DB
DB
PB
PB
PB
VB
\B
\B
bB
bB
bB
hB
hB
hB
hB
oB
uB
uB
uB
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
#�B
$�B
%�B
&�B
&�B
&�B
&�B
&�B
'�B
(�B
(�B
(�B
)�B
)�B
)�B
)�B
)�B
+B
,B
,B
,B
-B
-B
-B
-B
.B
.B
.B
.B
/B
0!B
0!B
0!B
1'B
2-B
2-B
2-B
33B
33B
49B
49B
5?B
5?B
5?B
5?B
5?B
5?B
6FB
7LB
9XB
9XB
9XB
9XB
:^B
:^B
:^B
:^B
:^B
:^B
:^B
:^B
:^B
:^B
:^B
:^B
;dB
:^B
;dB
;dB
=qB
>wB
>wB
?}B
?}B
@�B
@�B
@�B
A�B
A�B
A�B
A�B
B�B
B�B
B�B
B�B
B�B
B�B
D�B
E�B
E�B
E�B
E�B
F�B
F�B
F�B
F�B
F�B
G�B
G�B
G�B
G�B
G�B
H�B
H�B
I�B
I�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
K�B
L�B
L�B
L�B
L�B
L�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
N�B
N�B
N�B
O�B
N�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
P�B
P�B
P�B
P�B
Q�B
R�B
R�B
R�B
S�B
S�B
S�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
VB
VB
VB
VB
W
B
XB
YB
YB
ZB
ZB
ZB
ZB
ZB
[#B
[#B
[#B
[#B
[#B
[#B
\)B
\)B
\)B
\)B
]/B
]/B
^5B
^5B
^5B
^5B
^5B
^5B
^5B
_;B
_;B
_;B
_;B
`BB
`BB
`BB
aHB
aHB
aHB
aHB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
cTB
cTB
cTB
cTB
cTB
cTB
cTB
dZB
dZB
e`B
e`B
ffB
e`B
ffB
ffB
ffB
gmB
gmB
gmB
gmB
gmB
hsB
hsB
hsB
hsB
hsB
iyB
iyB
iyB
iyB
k�B
k�B
k�B
l�B
l�B
l�B
l�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
n�B
n�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
p�B
o�B
p�B
p�B
p�B
p�B
p�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
r�B
r�B
s�B
s�B
s�B
s�B
t�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111��B��B��B�B�4B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�sB��B�BkBXEBI�B<�B=VB6�B*�B�B"B��B�BؓB�B��B� B�-Bq�B`�BQhB4TB�B
��B
�5B
�2B
֡B
��B
�HB
w�B
iB
_!B
S�B
@�B
2�B
$@B
�B	��B	�kB	��B	��B	��B	��B	��B	��B	��B	��B	�9B	�B	�	B	�B	��B	�;B	�B	y�B	q[B	hXB	f�B	a�B	^B	]�B	\�B	[�B	Z�B	ZB	W�B	T�B	R�B	QB	M6B	G�B	EmB	?cB	:B	9>B	4�B	1�B	0�B	.B	+�B	$�B	�B	@B	�B	�B�$B�|B�UB� B��B�BΊB��B�?BĜB�B��B�B��B�?B�B�B��B�zB�TB�bB�HB�IB�B��B�(B��B��B��B��B��B��B�B��B�EB��B��B}�By�BwfBvFBu�Bu�Bs�Br|Bp�Bo�BlWBkBjBjeBh�BeBc�Bb�BbNBa�B]/BVBR:BRTBRoBQ�BPbBO\BNVBN<BM6BL~BKDBJrBI�BG�BFBC�BB'B>�B<�B:�B7�B6�B1�B/�B.�B-)B,�B+B)�B(�B'�B'RB%�B#�B"NB"hB!HB �B�B�BBEB�B�B�B}B�B�B�BBjBJB)BDB
�B
rB	�B	�B�B�BBB�B�B�B_B�B+B�BfB�B�B	�B
XB
rB
�B
�B�B@BdB#TB#�B#�B!�B"B$tB'B*B*�B+�B+�B+�B,qB-]B-wB-�B-�B.�B.�B.cB.}B.�B/�B1�B2�B4�B7�B;B=qB@OBAoBB�BFYBG_BIBJ	BJ#BJ�BL�BN�BP�BR�BV�BW�BY�B[#B]�BgmBkBlqBrBs�Bs�Bs�Bs�Bs�Bu?Bw2By>B{JB|jB|�B��B�EB�DB��B�B�gB��B��B�#B��B�5B��B��B�fB�sB�QB�CB�cB�IB�5B�;B�UB�oB��B��B��B��B��B��BżB��B�	B��B�B�B�HB�uB�yBیBݘB޸B��B��B��B�B��B��B�B�FB�rB	 OB	[B	aB	�B	zB		RB	
rB	^B	xB	dB	~B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	"4B	%zB	(XB	,"B	-)B	-)B	-CB	-CB	-B	.B	./B	.IB	./B	./B	.IB	.IB	/5B	/5B	/5B	/OB	/5B	/5B	/OB	0UB	1[B	1[B	1[B	2|B	3hB	3�B	4�B	8�B	9�B	<�B	@ B	C�B	E�B	F�B	GB	I�B	K�B	MB	M6B	OBB	QhB	T,B	VSB	XEB	Z7B	Z7B	[WB	\xB	]dB	^jB	_VB	`\B	abB	bhB	bhB	cnB	c�B	c�B	d�B	d�B	d�B	e�B	i�B	m�B	o�B	o�B	o�B	o�B	qB	t�B	v�B	xB	z*B	{0B	~B	.B	.B	�4B	�AB	�3B	�9B	�B	�9B	�SB	�SB	�YB	�YB	�fB	��B	��B	��B	�}B	��B	��B	��B	��B	��B	��B	�B	�B	�B	��B	��B	�B	�B	�B	�B	�
B	�*B	�DB	�B	�"B	�=B	�IB	�oB	��B	�tB	�zB	�lB	�rB	�XB	�XB	�XB	�XB	�rB	��B	�xB	��B	�B	��B	��B	��B	��B	��B	��B	��B	ªB	ªB	ðB	��B	ðB	��B	��B	��B	��B	�#B	�^B	�B	�4B	�&B	�B	�$B	�
B	�?B	�?B	�$B	�B	�+B	�_B	�QB	�WB	�IB	�dB	�dB	ބB	ޞB	�bB	�hB	�B	�TB	�nB	�TB	�B	�ZB	�B	�tB	�tB	�zB	�zB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	�B	��B	��B	�B	��B	�B	� B	�B	��B	��B	��B	��B	�	B	��B	��B	��B	��B	��B	�B	��B	��B	�6B	�B	�(B	�B	�HB
 B
;B
;B
'B
'B
GB
aB
9B
SB
tB
_B
fB
	RB

rB

XB
xB
�B
�B
jB
�B
pB
vB
vB
bB
}B
�B
hB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
B
�B
�B
�B
�B
 B
 �B
!�B
# B
#B
$B
%B
&B
&�B
'B
'B
'B
'8B
(
B
(�B
)*B
)B
*B
*B
*B
*B
*B
+B
,"B
,"B
,"B
-)B
-)B
-CB
-CB
.B
./B
.IB
.cB
/iB
0;B
0UB
0UB
1AB
2aB
2|B
2GB
3MB
3hB
4nB
4nB
5ZB
5tB
5tB
5?B
5ZB
5tB
6�B
7�B
9rB
9rB
9�B
9�B
:xB
:^B
:xB
:^B
:xB
:^B
:^B
:xB
:xB
:^B
:^B
:xB
;B
:�B
;�B
;�B
=�B
>�B
>�B
?�B
?�B
@�B
@�B
@�B
A�B
A�B
A�B
A�B
B�B
B�B
B�B
B�B
B�B
B�B
D�B
E�B
E�B
E�B
E�B
F�B
F�B
F�B
F�B
F�B
G�B
G�B
G�B
G�B
G�B
H�B
H�B
I�B
I�B
J�B
J�B
J�B
J�B
J�B
J�B
KB
K�B
L�B
L�B
L�B
L�B
L�B
M�B
M�B
NB
NB
M�B
NB
M�B
M�B
M�B
N�B
OB
N�B
O�B
OB
O�B
O�B
O�B
O�B
O�B
O�B
O�B
PB
QB
QB
QB
Q4B
R B
SB
SB
SB
TB
TB
TB
UB
T�B
T�B
U2B
UB
UB
UB
VB
V9B
VB
V9B
W?B
X_B
Y1B
YKB
Z7B
Z7B
Z7B
ZQB
Z7B
[#B
[WB
[=B
[=B
[=B
[=B
\CB
\CB
\CB
\CB
]IB
]dB
^jB
^jB
^OB
^5B
^OB
^5B
^OB
_;B
_VB
_VB
_VB
`vB
`vB
`\B
abB
abB
a|B
abB
bNB
bNB
bNB
b�B
bhB
bNB
bhB
bNB
bhB
b�B
cnB
cTB
cnB
cnB
c�B
cnB
c�B
d�B
d�B
ezB
e`B
f�B
e�B
f�B
f�B
f�B
g�B
g�B
g�B
g�B
g�B
h�B
hsB
hsB
h�B
h�B
i�B
i�B
i�B
i�B
k�B
k�B
k�B
l�B
l�B
l�B
l�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
n�B
n�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
p�B
o�B
p�B
p�B
p�B
p�B
p�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
r�B
r�B
s�B
s�B
s�B
s�B
t�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.08(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201904140032212019041400322120190414003221202211182138312022111821383120221118213831201904150018182019041500181820190415001818  JA  ARFMdecpA19c                                                                20190404003640  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20190403153642  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20190403153644  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20190403153644  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20190403153645  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20190403153645  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20190403153645  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20190403153645  QCP$                G�O�G�O�G�O�            FB40JA      jafc1.0                                                                 20190403153645                      G�O�G�O�G�O�                JA  ARUP                                                                        20190403155507                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20190403153127  CV  JULD            G�O�G�O�FŜ�                JM  ARCAJMQC2.0                                                                 20190413153221  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20190413153221  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20190414151818  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200114231520                      G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20221118123831  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20221123111508                      G�O�G�O�G�O�                