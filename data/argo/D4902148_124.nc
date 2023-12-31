CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       ~2018-01-08T15:35:33Z creation;2018-01-08T15:35:36Z conversion to V3.1;2019-12-18T07:25:42Z update;2022-11-21T05:31:25Z update;     
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
_FillValue                 �  ]l   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  aX   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  q   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �@   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �,   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �x   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �(   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �h   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �h   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �h   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �h   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �8   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �H   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �L   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �\   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �`   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �d   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �hArgo profile    3.1 1.2 19500101000000  20180108153533  20221123111507  4902148 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               |A   JA  I1_0397_124                     2C  Dd�NAVIS_A                         0397                            ARGO 011514                     863 @�C��1   @�C \�$ @;��%��2�d��8�Y1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  C   C  C  C  C  C
  C�fC  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�fC�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Du��Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D��D�#311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��
@�p�@�p�A�RA>�RA^�RA~�RA�\)A�\)A�\)A�\)A�\)A�\)A�\)A�\)B�B�B�B�B'�B/�B7�B?�BG�BO�BW�B_�Bg�Bo�Bw�B�B��
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
B�
=B��
B��
C�C�C�C�C	�C��C�C�C�C�C�C�C�C�C�C�C!�C#�C%�C'�C)�C+�C-�C/�C1�C3�C5�C7�C9�C;�C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C��C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D z�D ��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��D	z�D	��D
z�D
��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��D z�D ��D!z�D!��D"z�D"��D#z�D#��D$z�D$��D%z�D%��D&z�D&��D'z�D'��D(z�D(��D)z�D)��D*z�D*��D+z�D+��D,z�D,��D-z�D-��D.z�D.��D/z�D/��D0z�D0��D1z�D1��D2z�D2��D3z�D3��D4z�D4��D5z�D5��D6z�D6��D7z�D7��D8z�D8��D9z�D9��D:z�D:��D;z�D;��D<z�D<��D=z�D=��D>z�D>��D?z�D?��D@z�D@��DAz�DA��DBz�DB��DCz�DC��DDz�DD��DEz�DE��DFz�DF��DGz�DG��DHz�DH��DIz�DI��DJz�DJ��DKz�DK��DLz�DL��DMz�DM��DNz�DN��DOz�DO��DPz�DP��DQz�DQ��DRz�DR��DSz�DS��DTz�DT��DUz�DU��DVz�DV��DWz�DW��DXz�DX��DYz�DY��DZz�DZ��D[z�D[��D\z�D\��D]z�D]��D^z�D^��D_z�D_��D`z�D`��Daz�Da��Dbz�Db��Dcz�Dc��Ddz�Dd��Dez�De��Dfz�Df��Dgz�Dg��Dhz�Dh��Diz�Di��Djz�Dj��Dkz�Dk��Dlz�Dl��Dmz�Dm��Dnz�Dn��Doz�Do��Dpz�Dp��Dqz�Dq��Drz�Dr��Dsz�Ds��Dtz�Dt��Duz�Du�{Dvz�Dv��Dwz�Dw��Dxz�Dx��Dyz�Dy��Dzz�Dz��D{z�D{��D|z�D|��D}z�D}��D~z�D~��Dz�D��D�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��=D�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD� �D�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD½qD��qD�=qD�}qDýqD��qD�=qD�}qDĽqD��qD�=qD�}qDŽqD��qD�=qD�}qDƽqD��qD�=qD�}qDǽqD��qD�=qD�}qDȽqD��qD�=qD�}qDɽqD��qD�=qD�}qDʽqD��qD�=qD�}qD˽qD��qD�=qD�}qD̽qD��qD�=qD�}qDͽqD��qD�=qD�}qDνqD��qD�=qD�}qDϽqD��qD�=qD�}qDнqD��qD�=qD�}qDѽqD��qD�=qD�}qDҽqD��qD�=qD�}qDӽqD��qD�=qD�}qDԽqD��qD�=qD�}qDսqD��qD�=qD�}qDֽqD��qD�=qD�}qD׽qD��qD�=qD�}qDؽqD��qD�=qD�}qDٽqD��qD�=qD�}qDڽqD��qD�=qD�}qD۽qD��qD�=qD�}qDܽqD��qD�=qD�}qDݽqD��qD�=qD�}qD޽qD��qD�=qD�}qD߽qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD��qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD�
=D� �11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�ffA�jA�jA�l�A�jA�`BA�I�A�;dA�=qA�5?A�+A�&�A���A�ȴA��+A�`BA�VA�G�A�A�A�5?A�1'A�&�A�1A��TA���A��A��A�hsA�`BA�A�A�(�A�VA��A��/A���A��FA���A���A���A��A�hsA�^5A�ZA�Q�A�K�A�E�A�A�A�7LA�-A�$�A��A�bA��`A��wA��jA��^A�dZA���A�33A��+A��+A���A���A�XA�VA���A��#A�Q�A�/A�K�A���A�VA�\)A�p�A���A�(�A���A�{A�ĜA�A�=qA�A�1'A��A�#A|��A{�#A{?}AzffAy�Aw��Ar�/Am�-Al��Ak�mAkK�Aj��AjbNAi��Ai�wAi�PAip�AiC�Ai�Ah�AhjAh(�Ag�Af5?Ad�/AdJAc��AcoAbE�AaO�A_\)A]�#A\�\AY�AW
=AT  AS`BASVAQ�wAP1AOG�ANĜANjANn�ANffANjANZAN-AM��AL��ALJAK�-AK��AKhsAJ�AJ�AHffAHAGl�AE�AD^5ACt�ABQ�AA�AA�PA@��A?��A?S�A>��A>�A>=qA=��A<��A<�\A<A�A;?}A9��A9|�A9�A8I�A7l�A6�A6{A4$�A3dZA3A3%A3oA3
=A2$�A0z�A/l�A.�A-�FA-;dA,�HA,ffA+��A*�RA)��A(I�A'��A'��A&�RA&E�A&-A& �A&JA%��A%l�A%VA$�A$bA#XA"�uA!t�A -AO�A�\A�+A �A�PAO�A��AjA9XA�`A��Ax�A\)A/A�AoA�A�A��A�jA��A�jA�\A�A�hA�#A
�A	S�AVA9XA��A~�A5?A�wA\)A&�AA�uA�A�^A33A J@��^@�33@�V@�9X@�C�@�hs@���@�\@��@���@��m@��
@�ƨ@�+@��@���@�hs@��@�\)@�33@�33@�o@�@��@޸R@އ+@�@�(�@��`@���@��@���@Ձ@�j@��@��@�+@͡�@ˮ@���@���@�j@�I�@�{@���@��-@�1'@�n�@���@�1'@��F@��@�@��@�S�@��@��@�~�@�v�@�n�@�^5@�{@�O�@��w@�+@�;d@�33@�@�@�&�@��/@� �@�dZ@��y@�^5@�$�@��T@�G�@���@�t�@�
=@��@�G�@��@���@��D@���@���@��@�`B@���@���@���@�Z@�I�@�1'@�1@�dZ@���@�^5@�x�@��@�dZ@��#@�O�@��u@��P@�5?@�7L@��@�A�@��P@�+@�$�@�G�@�z�@��;@�l�@�S�@�C�@�
=@���@�M�@�$�@��T@��7@�?}@��@���@��/@�Ĝ@��9@��u@�r�@�I�@�b@��
@���@�dZ@�C�@��@�@���@�n�@�$�@��@��-@�/@��@�z�@�I�@��;@�"�@��+@�V@��@�p�@�V@���@��@���@�r�@�j@�r�@�j@�I�@�(�@���@���@�33@�"�@�@��y@�ff@�@���@��@�O�@��@���@��/@�r�@���@�+@��!@�5?@��h@�7L@��@��`@�Ĝ@��j@���@���@���@�z�@�Z@�;@~��@~@}`B@}/@|�@|I�@|�@|�@|(�@|�@|1@{�m@{�@{o@zn�@yhs@x��@xĜ@x  @w�@v5?@v$�@u��@t�@t�@s�F@st�@s33@s@r�@s@so@so@r��@rn�@r^5@r=q@r�@q��@q�#@q�^@q�7@q7L@p�9@p�@p1'@o�w@o��@oK�@n��@n��@n{@m�T@mp�@mV@mV@mV@l��@l��@l�@l�@l�@l�@l�@l�@l�@l�@l�@l�@l�@l�@l�@l��@l�@l�@l�@l�/@l�@lZ@k�m@kC�@jn�@i�@h�@hbN@h1'@g�@g�w@g��@g��@g|�@g
=@f5?@e��@e��@e`B@eV@d�j@dI�@c��@c33@b�@b�\@b-@a�@a��@a�^@ax�@ahs@aG�@a7L@a%@`�`@`��@`��@`��@`��@`�`@a�@a7L@ahs@ax�@aX@aX@a%@`��@`��@`Ĝ@`��@`�@`bN@`A�@_\)@_
=@^��@^��@]�@]�@\��@[�m@[t�@[33@Z��@Z��@ZM�@Y��@X�u@Xb@W+@V��@VE�@U�@U`B@T�/@T�@T�@T�@T�@T�@Tj@T�@S�m@R��@Q7L@P��@P�`@P��@P�u@P1'@O�P@O
=@Nff@M�T@M��@LZ@K�m@Kƨ@K��@K�@KS�@K33@K@J��@J�!@J�!@J��@Jn�@JM�@J-@J�@I��@I��@I�7@IX@IX@I�@H�@H �@G��@G�@F�@F�+@E�@EV@D��@Dz�@Dj@DZ@D9X@D(�@D9X@D�@C��@B~�@B�@A�#@A��@A�7@Ahs@A7L@@�u@?�;@?�;@?��@?��@?K�@?;d@?+@?
=@>�R@>V@>E�@>@=`B@<�/@<�D@<j@<I�@<(�@<�@;ƨ@;�F@;��@;t�@;S�@:�!@9��@97L@8�@7\)@6�y@6��@6�+@6ff@65?@5�@5�-@5p�@5?}@5V@5V@4��@4�/@4��@4�@4�D@4z�@4�D@4�D@4z�@4j@4j@4j@49X@4(�@4�@3�F@3C�@2�!@2~�@2�@1�@1�^@1��@1G�@0bN@/|�@/�@.��@.�y@.�y@.�R@.��@.��@.�+@.V@-�@-��@-�-@-p�@,�@+��@+dZ@+C�@+33@+33@+33@+"�@*�!@*=q@)��@)�^@)�7@(Ĝ@(b@'��@'l�@'\)@'K�@';d@'
=@&�R@%�@%�-@%�h@%p�@%?}@$��@$�@$��@$�j@$�D@$(�@#�
@#��@#��@#�@#t�@#S�@#"�@"�H@"��@"��@"M�@!�#@!�7@!G�@ �`@ �@  �@ b@   @�@��@�@�@��@��@|�@K�@;d@+@�y@�@��@v�@E�@@�-@p�@`B@`B@?}@��@�@��@�D@Z@Z@9X@�@1@�F@33@�!@n�@��@x�@7L@��@��@�u@r�@Q�@ �@b@  @�@�;@�@l�@;d@+@��@�R@$�@�T@�@V@�@��@�@��@��@�@�/@�j@�D@Z@ƨ@dZ@S�@33@�H@�@��@x�@G�@�@��@�u@�@bN@b@��@�P@+@
=@��@�y@�y@�y@�R@��@�+@�+@�+@v�@�+@ff@E�@�T@@�@p�@?}@�@�@�@V@�/@Z@�m@ƨ@�
@�
@�
@ƨ@�F@��@t�@S�@C�@
�!@
n�@
=q@
J@	��@	��@	��@	x�@	X@	7L@	&�@	&�@	&�@	�@Ĝ@r�@bN@bN@Q�@b@�@�@|�@;d@�y@�@ȴ@�+@E�@E�@5?@{@�@��@@@@�-@�@O�@?}@�@�@�D@z�@j@Z@j@Z@Z@Z@�@�m@�
@��@S�@o@@�@��@�H@�H@��@�\@^5@=q@��@��@x�@7L@%@ ��@ bN@ A�@ b@ b@   @   @   ?��w?��w?�\)?��R?���?��-?��-?�p�?�/?�/11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�ffA�jA�jA�l�A�jA�`BA�I�A�;dA�=qA�5?A�+A�&�A���A�ȴA��+A�`BA�VA�G�A�A�A�5?A�1'A�&�A�1A��TA���A��A��A�hsA�`BA�A�A�(�A�VA��A��/A���A��FA���A���A���A��A�hsA�^5A�ZA�Q�A�K�A�E�A�A�A�7LA�-A�$�A��A�bA��`A��wA��jA��^A�dZA���A�33A��+A��+A���A���A�XA�VA���A��#A�Q�A�/A�K�A���A�VA�\)A�p�A���A�(�A���A�{A�ĜA�A�=qA�A�1'A��A�#A|��A{�#A{?}AzffAy�Aw��Ar�/Am�-Al��Ak�mAkK�Aj��AjbNAi��Ai�wAi�PAip�AiC�Ai�Ah�AhjAh(�Ag�Af5?Ad�/AdJAc��AcoAbE�AaO�A_\)A]�#A\�\AY�AW
=AT  AS`BASVAQ�wAP1AOG�ANĜANjANn�ANffANjANZAN-AM��AL��ALJAK�-AK��AKhsAJ�AJ�AHffAHAGl�AE�AD^5ACt�ABQ�AA�AA�PA@��A?��A?S�A>��A>�A>=qA=��A<��A<�\A<A�A;?}A9��A9|�A9�A8I�A7l�A6�A6{A4$�A3dZA3A3%A3oA3
=A2$�A0z�A/l�A.�A-�FA-;dA,�HA,ffA+��A*�RA)��A(I�A'��A'��A&�RA&E�A&-A& �A&JA%��A%l�A%VA$�A$bA#XA"�uA!t�A -AO�A�\A�+A �A�PAO�A��AjA9XA�`A��Ax�A\)A/A�AoA�A�A��A�jA��A�jA�\A�A�hA�#A
�A	S�AVA9XA��A~�A5?A�wA\)A&�AA�uA�A�^A33A J@��^@�33@�V@�9X@�C�@�hs@���@�\@��@���@��m@��
@�ƨ@�+@��@���@�hs@��@�\)@�33@�33@�o@�@��@޸R@އ+@�@�(�@��`@���@��@���@Ձ@�j@��@��@�+@͡�@ˮ@���@���@�j@�I�@�{@���@��-@�1'@�n�@���@�1'@��F@��@�@��@�S�@��@��@�~�@�v�@�n�@�^5@�{@�O�@��w@�+@�;d@�33@�@�@�&�@��/@� �@�dZ@��y@�^5@�$�@��T@�G�@���@�t�@�
=@��@�G�@��@���@��D@���@���@��@�`B@���@���@���@�Z@�I�@�1'@�1@�dZ@���@�^5@�x�@��@�dZ@��#@�O�@��u@��P@�5?@�7L@��@�A�@��P@�+@�$�@�G�@�z�@��;@�l�@�S�@�C�@�
=@���@�M�@�$�@��T@��7@�?}@��@���@��/@�Ĝ@��9@��u@�r�@�I�@�b@��
@���@�dZ@�C�@��@�@���@�n�@�$�@��@��-@�/@��@�z�@�I�@��;@�"�@��+@�V@��@�p�@�V@���@��@���@�r�@�j@�r�@�j@�I�@�(�@���@���@�33@�"�@�@��y@�ff@�@���@��@�O�@��@���@��/@�r�@���@�+@��!@�5?@��h@�7L@��@��`@�Ĝ@��j@���@���@���@�z�@�Z@�;@~��@~@}`B@}/@|�@|I�@|�@|�@|(�@|�@|1@{�m@{�@{o@zn�@yhs@x��@xĜ@x  @w�@v5?@v$�@u��@t�@t�@s�F@st�@s33@s@r�@s@so@so@r��@rn�@r^5@r=q@r�@q��@q�#@q�^@q�7@q7L@p�9@p�@p1'@o�w@o��@oK�@n��@n��@n{@m�T@mp�@mV@mV@mV@l��@l��@l�@l�@l�@l�@l�@l�@l�@l�@l�@l�@l�@l�@l�@l��@l�@l�@l�@l�/@l�@lZ@k�m@kC�@jn�@i�@h�@hbN@h1'@g�@g�w@g��@g��@g|�@g
=@f5?@e��@e��@e`B@eV@d�j@dI�@c��@c33@b�@b�\@b-@a�@a��@a�^@ax�@ahs@aG�@a7L@a%@`�`@`��@`��@`��@`��@`�`@a�@a7L@ahs@ax�@aX@aX@a%@`��@`��@`Ĝ@`��@`�@`bN@`A�@_\)@_
=@^��@^��@]�@]�@\��@[�m@[t�@[33@Z��@Z��@ZM�@Y��@X�u@Xb@W+@V��@VE�@U�@U`B@T�/@T�@T�@T�@T�@T�@Tj@T�@S�m@R��@Q7L@P��@P�`@P��@P�u@P1'@O�P@O
=@Nff@M�T@M��@LZ@K�m@Kƨ@K��@K�@KS�@K33@K@J��@J�!@J�!@J��@Jn�@JM�@J-@J�@I��@I��@I�7@IX@IX@I�@H�@H �@G��@G�@F�@F�+@E�@EV@D��@Dz�@Dj@DZ@D9X@D(�@D9X@D�@C��@B~�@B�@A�#@A��@A�7@Ahs@A7L@@�u@?�;@?�;@?��@?��@?K�@?;d@?+@?
=@>�R@>V@>E�@>@=`B@<�/@<�D@<j@<I�@<(�@<�@;ƨ@;�F@;��@;t�@;S�@:�!@9��@97L@8�@7\)@6�y@6��@6�+@6ff@65?@5�@5�-@5p�@5?}@5V@5V@4��@4�/@4��@4�@4�D@4z�@4�D@4�D@4z�@4j@4j@4j@49X@4(�@4�@3�F@3C�@2�!@2~�@2�@1�@1�^@1��@1G�@0bN@/|�@/�@.��@.�y@.�y@.�R@.��@.��@.�+@.V@-�@-��@-�-@-p�@,�@+��@+dZ@+C�@+33@+33@+33@+"�@*�!@*=q@)��@)�^@)�7@(Ĝ@(b@'��@'l�@'\)@'K�@';d@'
=@&�R@%�@%�-@%�h@%p�@%?}@$��@$�@$��@$�j@$�D@$(�@#�
@#��@#��@#�@#t�@#S�@#"�@"�H@"��@"��@"M�@!�#@!�7@!G�@ �`@ �@  �@ b@   @�@��@�@�@��@��@|�@K�@;d@+@�y@�@��@v�@E�@@�-@p�@`B@`B@?}@��@�@��@�D@Z@Z@9X@�@1@�F@33@�!@n�@��@x�@7L@��@��@�u@r�@Q�@ �@b@  @�@�;@�@l�@;d@+@��@�R@$�@�T@�@V@�@��@�@��@��@�@�/@�j@�D@Z@ƨ@dZ@S�@33@�H@�@��@x�@G�@�@��@�u@�@bN@b@��@�P@+@
=@��@�y@�y@�y@�R@��@�+@�+@�+@v�@�+@ff@E�@�T@@�@p�@?}@�@�@�@V@�/@Z@�m@ƨ@�
@�
@�
@ƨ@�F@��@t�@S�@C�@
�!@
n�@
=q@
J@	��@	��@	��@	x�@	X@	7L@	&�@	&�@	&�@	�@Ĝ@r�@bN@bN@Q�@b@�@�@|�@;d@�y@�@ȴ@�+@E�@E�@5?@{@�@��@@@@�-@�@O�@?}@�@�@�D@z�@j@Z@j@Z@Z@Z@�@�m@�
@��@S�@o@@�@��@�H@�H@��@�\@^5@=q@��@��@x�@7L@%@ ��@ bN@ A�@ b@ b@   @   @   ?��w?��w?�\)?��R?���?��-?��-?�p�?�/?�/11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B#�B#�B#�B#�B#�B"�B �B�B�B�B�B�B�B�BoBbBbBbBhBoB{B�B�B{BoBhB\BuBuB�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B{BPBB�B�TBɺB�1BW
B�B1BB��B�B�wB�hBx�BdZBR�BD�B9XB.B�B\B	7B
��B
�B
�mB
��B
��B
m�B
[#B
S�B
P�B
L�B
G�B
=qB
.B
bB
DB
+B
B
B
B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�yB	�NB	�
B	��B	ǮB	�}B	�?B	��B	�oB	~�B	m�B	I�B	7LB	'�B	.B	-B	%�B	�B	�B	oB	VB	VB	PB	PB	JB	DB	1B	1B	JB	bB	�B	�B	�B	"�B	.B	0!B	-B	'�B	$�B	 �B	�B	�B	�B	uB	bB	PB	DB	
=B		7B	%B	B	B��B��B�B�B�B�yB�ZB�BB�#B�
B��B��B��B��B��B��B��BǮBĜB��B�wB�qB�dB�RB�?B�'B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B�{B�bB�VB�7B�B}�B|�Bz�Bv�Bp�Bk�BhsBe`Be`BdZBdZBdZBcTBcTBcTBbNBaHB^5B\)B[#BYBW
BR�BO�BL�BI�BE�BC�BC�BB�BA�BA�B@�B@�B>wB=qB<jB:^B8RB6FB49B33B33B2-B0!B0!B/B.B.B.B.B-B,B)�B(�B$�B&�B'�B'�B'�B'�B'�B'�B&�B&�B$�B$�B&�B(�B(�B'�B&�B%�B%�B%�B&�B'�B(�B)�B+B,B,B-B0!B0!B33B5?B8RB@�BB�BC�BE�BI�BK�BL�BL�BM�BM�BM�BM�BM�BM�BQ�BR�BR�BR�BR�BVBW
BW
BYB[#B\)B]/B]/B]/B^5BbNBbNBcTBe`BhsBiyBiyBiyBk�Bn�Bp�Br�Bs�Bt�Bt�Bu�Bv�Bv�Bv�Bx�By�Bz�B}�B|�B�B�1B�=B�PB�uB��B��B��B��B��B��B�B�'B�FB�XB�dB�jB�jB�qB�}B��BBĜBƨBǮBȴBɺBɺB��B��B��B��B��B��B��B��B��B��B��B��B�
B�B�#B�)B�/B�BB�ZB�`B�`B�sB�B�B�B��B��B��B��B��B	  B	B	B	B	B	B	B	B	B		7B	
=B	JB	JB	hB	{B	�B	�B	�B	�B	�B	�B	�B	&�B	+B	.B	2-B	7LB	:^B	;dB	=qB	>wB	>wB	>wB	?}B	?}B	?}B	@�B	C�B	I�B	K�B	M�B	N�B	P�B	R�B	S�B	T�B	T�B	T�B	T�B	VB	W
B	YB	[#B	_;B	`BB	aHB	cTB	ffB	iyB	iyB	jB	l�B	o�B	p�B	p�B	r�B	r�B	s�B	s�B	r�B	r�B	s�B	u�B	u�B	v�B	v�B	w�B	w�B	w�B	x�B	z�B	{�B	|�B	}�B	� B	�B	�B	�B	�B	�1B	�1B	�DB	�VB	�VB	�\B	�\B	�\B	�\B	�bB	�bB	�bB	�bB	�bB	�bB	�bB	�bB	�bB	�hB	�hB	�hB	�oB	�oB	�oB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�!B	�'B	�'B	�-B	�-B	�3B	�9B	�9B	�?B	�FB	�LB	�LB	�RB	�XB	�XB	�^B	�^B	�dB	�jB	�qB	�qB	�qB	�wB	�wB	�}B	��B	ÖB	ŢB	ŢB	ŢB	ƨB	ƨB	ǮB	ǮB	ǮB	ȴB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�)B	�)B	�5B	�BB	�BB	�BB	�HB	�NB	�NB	�NB	�NB	�NB	�NB	�TB	�ZB	�ZB	�mB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
+B
1B
	7B

=B

=B

=B

=B

=B

=B

=B

=B
PB
VB
\B
\B
\B
\B
bB
hB
uB
uB
uB
uB
{B
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
"�B
$�B
%�B
&�B
&�B
&�B
&�B
'�B
'�B
'�B
'�B
(�B
(�B
(�B
(�B
(�B
(�B
)�B
)�B
)�B
)�B
)�B
)�B
)�B
)�B
)�B
)�B
)�B
+B
+B
,B
,B
-B
-B
-B
-B
-B
/B
1'B
1'B
2-B
2-B
2-B
2-B
2-B
33B
33B
33B
49B
49B
49B
49B
5?B
7LB
8RB
8RB
8RB
9XB
9XB
9XB
:^B
;dB
<jB
<jB
<jB
=qB
>wB
?}B
?}B
?}B
?}B
?}B
?}B
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
B�B
C�B
C�B
C�B
C�B
C�B
D�B
D�B
D�B
D�B
D�B
D�B
E�B
F�B
F�B
G�B
G�B
H�B
H�B
H�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
J�B
J�B
J�B
J�B
K�B
K�B
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
O�B
O�B
O�B
O�B
O�B
O�B
O�B
P�B
Q�B
Q�B
Q�B
R�B
S�B
T�B
T�B
T�B
T�B
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
VB
W
B
XB
XB
YB
YB
YB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
[#B
\)B
\)B
\)B
]/B
]/B
^5B
_;B
_;B
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
cTB
cTB
cTB
cTB
dZB
dZB
dZB
e`B
e`B
dZB
dZB
e`B
e`B
ffB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
hsB
hsB
iyB
iyB
iyB
iyB
jB
jB
jB
jB
jB
jB
jB
jB
k�B
k�B
k�B
k�B
k�B
l�B
l�B
l�B
l�B
m�B
m�B
n�B
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
o�B
o�B
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
r�B
r�B
r�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
t�B
t�B
t�B
t�B
u�B
u�B
u�B
v�B
v�B
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
y�B
y�B
y�B
z�B
z�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B#�B#�B#�B$B#�B# B �B�B�B�B�B/BBB�B}B�B}B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�BgB�B�B��B�BϑB��B]dB�B	RBAB B��B��B�gB{�Bf�BU2BFYB;0B0�B~B�BB  B
�B
�qB
�B
��B
p�B
\]B
T�B
R B
N�B
K)B
CB
2�B
�B
JB
�B
�B
�B
�B	�BB	�6B	�B	�*B	�>B	�8B	�zB	�TB	��B	��B	��B	�B	ΊB	ȚB	��B	��B	�=B	��B	�;B	q'B	MPB	:^B	(�B	.�B	.�B	'�B	�B	+B	�B	pB	�B	jB	�B	�B	B		RB		B	�B	�B	B	�B	!B	$�B	.�B	1vB	/OB	)_B	&B	!�B	]B	kB	�B	{B	4B	B	�B	
�B		�B	_B	�B	�B�}B�XB�nB�aB�B�B�`B�B�/B�BԕB�,B�2B՛BՁB��B�6BȚB��B�UB�B�]B��B��B��B��B��B��B�B�sB�B�B�LB�LB��B�ZB�TB�B��B��B�7B�9B��B�B��B��B~�B}�B|jBy�BsMBmCBi�Be�Be�Bd�Bd�Bd�Bc�Bc�Bc�Bb�Bb�B_�B\�B\)BZ7BYKBT�BQ�BN�BLdBG_BDBDBCaBBABBBABAUB?HB>(B=�B<B:B8B5�B4B4B3�B1AB1'B0;B.�B.IB.IB.cB-�B-�B+�B+B)_B'�B(>B($B($B(>B(>B($B'mB'�B&�B&�B($B)yB)yB(sB'�B'8B'8B'mB(>B)_B*B*�B+�B,�B-�B/5B1B1[B4nB6FB8�B@�BC-BD�BF�BJ=BK�BMBM6BM�BM�BNBNVBN�BN�BR:BS&BS&BS[BS�BV�BWsBW�BY�B[�B\�B]~B]�B]�B_;Bb�Bb�BdBe�Bh�Bi�Bi�BjBlqBo5Bq'Br�Bs�Bt�Bt�Bu�Bv�BwBwfByXBz�B{�B~�B~(B�B��B��B�VB�aB�QB�!B�HB�nB��B��B��B��B��B��B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B� B�@B�FB�MB�MB�YB�B�WB�xBݲB�B�B�B��B��B�B��B�B�2B�0B�B�"B�BB	 OB	 B	;B	 B	;B	AB	aB	{B	mB		RB	
rB	~B	�B	�B	�B	�B	�B	�B	�B	�B	/B	 \B	'8B	+�B	.}B	2�B	7�B	:�B	;�B	=�B	>�B	>�B	>�B	?�B	?�B	?�B	@�B	DB	I�B	LB	M�B	O(B	Q4B	SB	S�B	T�B	U2B	UB	UB	VSB	WYB	YB	[�B	_�B	`vB	a�B	c�B	f�B	i�B	i�B	j�B	l�B	o�B	p�B	p�B	r�B	r�B	s�B	s�B	r�B	r�B	s�B	u�B	u�B	v�B	v�B	w�B	w�B	w�B	y	B	{B	|B	}<B	~BB	�B	�UB	�AB	�aB	�SB	�fB	�fB	�^B	�VB	��B	�vB	�vB	�vB	�vB	�}B	�bB	�bB	�}B	�bB	�}B	�bB	�}B	�bB	�hB	��B	�hB	�oB	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	��B	��B	�B	��B	�
B	�B	�B	�=B	�wB	�IB	�5B	�UB	�[B	�vB	�aB	�aB	�hB	�TB	��B	��B	�zB	��B	��B	��B	�rB	�rB	�xB	�xB	��B	��B	�qB	��B	�qB	��B	�wB	�}B	��B	ÖB	żB	żB	żB	��B	��B	��B	��B	��B	��B	��B	�	B	�B	�B	�B	�"B	�.B	�.B	�NB	�,B	�B	�MB	�B	�?B	�_B	ڠB	�]B	ܒB	ބB	�vB	�vB	��B	�B	�hB	�NB	�hB	�NB	�B	�hB	�B	�B	��B	��B	�B	�B	��B	�B	��B	��B	��B	��B	��B	�B	�9B	�B	��B	��B	��B	��B	��B	�B	�B	�B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�HB
 OB
UB
AB
3B
MB
gB
_B
KB
	RB

XB

XB

XB

=B

XB

rB

�B

�B
jB
pB
�B
vB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
B
 B
!B
#TB
%,B
%�B
'B
'B
'B
'B
(
B
($B
($B
(
B
(�B
)B
)B
)B
)*B
)*B
*B
)�B
)�B
*B
)�B
*B
)�B
*0B
*0B
*B
*KB
+QB
+6B
,=B
,=B
-)B
-)B
-)B
-]B
-wB
/�B
1AB
1AB
2-B
2GB
2GB
2-B
2aB
3MB
3MB
3MB
4TB
4TB
4�B
4�B
5�B
7�B
8RB
8RB
8lB
9XB
9rB
9�B
:�B
;�B
<�B
<�B
<�B
=�B
>�B
?�B
?}B
?�B
?�B
?�B
?�B
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
B�B
C�B
C�B
C�B
C�B
C�B
D�B
D�B
D�B
D�B
D�B
D�B
E�B
F�B
F�B
G�B
G�B
H�B
H�B
H�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
J�B
J�B
J�B
J�B
K�B
K�B
L�B
L�B
L�B
M�B
N�B
N�B
N�B
N�B
OB
N�B
OB
O�B
O�B
PB
O�B
O�B
P.B
PB
Q4B
RB
R:B
R B
S&B
TB
T�B
UB
U2B
UB
UB
T�B
UB
T�B
U2B
UB
UB
V9B
V9B
VB
VB
VSB
W?B
X+B
XEB
YB
Y1B
Y1B
Z7B
ZB
Z7B
Z7B
ZQB
Z7B
Z7B
[qB
\CB
\CB
\]B
]~B
]~B
^OB
_pB
_VB
_VB
`vB
`\B
`\B
`vB
a|B
abB
a|B
a|B
bhB
bhB
bNB
bhB
bhB
b�B
cTB
cnB
cTB
cTB
cnB
cTB
c�B
c�B
cnB
cnB
cnB
dtB
d�B
d�B
ezB
ezB
d�B
dtB
e�B
e�B
ffB
gmB
gmB
gmB
gmB
g�B
g�B
g�B
g�B
g�B
g�B
h�B
h�B
i�B
i�B
i�B
i�B
j�B
j�B
j�B
j�B
jB
jB
j�B
j�B
k�B
k�B
k�B
k�B
k�B
l�B
l�B
l�B
l�B
m�B
m�B
n�B
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
o�B
o�B
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
r�B
r�B
r�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
t�B
t�B
t�B
t�B
u�B
u�B
u�B
v�B
v�B
xB
w�B
w�B
w�B
w�B
w�B
xB
x�B
y	B
y	B
x�B
y�B
y�B
zB
y�B
z�B
z�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.08(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201801190033292018011900332920180119003329202211182133132022111821331320221118213313201804031938502018040319385020180403193850  JA  ARFMdecpA19c                                                                20180109003511  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180108153533  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180108153534  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180108153535  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180108153535  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180108153535  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180108153535  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180108153535  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20180108153536  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20180108153536                      G�O�G�O�G�O�                JA  ARUP                                                                        20180108155551                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180108153117  CV  JULD            G�O�G�O�F��                JM  ARCAJMQC2.0                                                                 20180118153329  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180118153329  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180403103850  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200114171535                      G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20221118123313  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20221123111507                      G�O�G�O�G�O�                