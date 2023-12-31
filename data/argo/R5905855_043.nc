CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T19:18:13Z creation;2022-06-04T19:18:13Z conversion to V3.1      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  78   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7x   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     7�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8<   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8@   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8H   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8L   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8T   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8\   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8d   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8h   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8p   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8t   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8x   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8|   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9|   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I4   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M$   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p|   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tl   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �h   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �X   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �t   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ޤ   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �    HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �0   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �4   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �D   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �H   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �L   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �PArgo profile    3.1 1.2 19500101000000  20220604191813  20220610151508  5905855                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               +A   JA                                  2B  A   APEX                            8423                            2.11.2                          846 @�gY =1   @��ʆ@/$�/�cup��
=1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @33@�  @�  A   A   AA��A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�33B�  B�  B�33B���B�  B�  B�  B���B�  B�  B�  B�  B�  Bę�B�  B�  B�  B�  Bי�B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C�C��C  C�fC
  C  C  C  C  C  C  C  C  C�C  C   C"  C$�C&33C'�fC)�fC,  C-�fC0  C2  C3�fC5�fC8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR�CT�CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{fD{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�)�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @{@z�H@�p�@�p�A�RA@Q�A^�RA~�RA�\)A�\)A�\)A�\)A�\)A�\)A�\)A�\)B�B�B�B�B'�B/�B7�B?�BG�BO�BW�B_�Bg�Bo�Bw�B�B��
B��
B�
=B��
B��
B�
=B���B��
B��
B��
B���B��
B��
B��
B��
B��
B�p�B��
B��
B��
B��
B�p�B��
B��
B��
B��
B��
B��
B��
B��
B��
B��
CC�RC�C��C	�C�C�C�C�C�C�C�C�CC�C�C!�C$C&�C'��C)��C+�C-��C/�C1�C3��C5��C7�C9�C;�C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�CRCTCU�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���D z�D ��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��D	z�D	��D
z�D
��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��D z�D ��D!z�D!��D"z�D"��D#z�D#��D$z�D$��D%z�D%��D&z�D&��D'z�D'��D(z�D(��D)z�D)��D*z�D*��D+z�D+��D,z�D,��D-z�D-��D.z�D.��D/z�D/��D0z�D0��D1z�D1��D2z�D2��D3z�D3��D4z�D4��D5z�D5��D6z�D6��D7z�D7��D8z�D8��D9z�D9��D:z�D:��D;z�D;��D<z�D<��D=z�D=��D>z�D>��D?z�D?��D@z�D@��DAz�DA��DBz�DB��DCz�DC��DDz�DD��DEz�DE��DFz�DF��DGz�DG��DHz�DH��DIz�DI��DJz�DJ��DKz�DK��DLz�DL��DMz�DM��DNz�DN��DOz�DO��DPz�DP��DQz�DQ��DRz�DR��DSz�DS��DTz�DT��DUz�DU��DVz�DV��DWz�DW��DXz�DX��DYz�DY��DZz�DZ��D[z�D[��D\z�D\��D]z�D]��D^z�D^��D_z�D_��D`z�D`��Daz�Da��Dbz�Db��Dcz�Dc��Ddz�Dd��Dez�De��Dfz�Df��Dgz�Dg��Dhz�Dh��Diz�Di��Djz�Dj��Dkz�Dk��Dlz�Dl��Dmz�Dm��Dnz�Dn��Doz�Do��Dpz�Dp��Dqz�Dq��Drz�Dr��Dsz�Ds��Dtz�Dt��Duz�Du��Dvz�Dv��Dwz�Dw��Dxz�Dx��Dyz�Dy��Dzz�D{GD{z�D{��D|z�D|��D}z�D}��D~z�D~��Dz�D��D�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�:>D�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD���D��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD� �D�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD½qD��qD�=qD�}qDýqD��qD�=qD�}qDĽqD��qD�=qD�}qDŽqD��qD�=qD�}qDƽqD��qD�=qD�}qDǽqD��qD�=qD�}qDȽqD��qD�=qD�}qDɽqD��qD�=qD�}qDʽqD��qD�=qD�}qD˽qD��qD�=qD�}qD̽qD��qD�=qD�}qDͽqD��qD�=qD�}qDνqD��qD�=qD�}qDϽqD��qD�=qD�}qDнqD��qD�=qD�}qDѽqD��qD�=qD�}qDҽqD��qD�=qD�}qDӽqD��qD�=qD�}qDԽqD��qD�=qD�}qDսqD��qD�=qD�}qDֽqD��qD�=qD�}qD׽qD��qD�=qD�}qDؽqD��qD�=qD�}qDٽqD��qD�=qD�}qDڽqD��qD�=qD�}qD۽qD��qD�=qD�}qDܽqD��qD�=qD�}qDݽqD��qD�=qD�}qD޽qD��qD�=qD�}qD߽qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD��qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�'111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A֠'A�^�A�s�A�MA�LdAұ�A�M�A��#A���AН~A�s�Aς�A���A�'�A�'RAˏ�A��A�XA�VA�˒AɑhAɅ�A�aA�W�A�d�A�.IA�A��AȹXA�~(A�r�A�rA�J#A�AA��VA��A��A�ߤAǩ�AǇ�A�k�A�d&A�XEA�=A�A��AŮ}A�F�Aó�A���A��IA�Y�A��6A��dA���A��A��A�d�A��zA�V�A�cA�L�A�-�A�kA�JXA��A�LdA��A�}�A��A��NA���A��)A�4nA��WA�A A�A���A�[�A���A��"A}e,Aw��Ap�WAl�"Aj�Ag�kAbn/A`qA^b�A\TaA[��AZz�AY�AW�AWOvAU�AR�AP�6APqAL+�AH��AFAD�OACh
A?\)A=�A;��A:r�A7DgA4bNA4��A4OA3҉A2}�A/�VA.��A-PHA,��A,��A,��A,I�A++A*<6A)��A)v`A(OvA'S&A&�A'U�A'9XA&
�A$��A!�\A2aAqA�uA�DA��A2aA�A-A&AQ�AA A0�A�A�A�jA��A}�An�AVAE�A�Am]A,=A�.A�,A�VAVA{�A�AffAC�A�KA�fA-�A�1A \A��A\)A�xAP�A(A�AA��Ac�A�A�6Ap�A��A
�zA	��A�A�TAe�A�9A��A
�A��A@�A%�A�TA��A�YA��A]dA]dA֡AA5�AE9A��Ae�AJA c A �@��W@��@��@�!�@���@���@��0@�H@�ƨ@�e,@��@�خ@��t@���@� \@��F@�7@��g@���@���@��"@�,�@�+@�&�@��@�h@��@�C@�S@�ی@�"h@�q@��@�}�@�"�@��@��@�*�@��K@�N<@ꀝ@�$@�{@�@@��@��c@� @�w�@�G@�s@��[@䤩@�:*@��9@��@�S@�[W@��8@�	@��2@�-�@��@�O@�H@�g�@��@ޗ�@�9X@ݹ�@�K�@��f@ܶ�@�s�@ۚk@�ѷ@��@ن�@��@�~(@��@ץ�@�S&@ֆY@�b@��D@�ƨ@�7L@��'@Ԁ�@��@�H�@�J�@��@��@у{@��8@��Z@�ں@�@�o�@���@̚�@�:*@���@�o�@ʌ�@�($@��#@ə�@�>�@�E9@���@�c�@�1'@��T@�+�@Ʈ}@���@���@���@Ļ�@�H@Úk@�҉@�9X@��>@�>�@��@�Ĝ@�z@�-�@��3@�n/@�H�@�%F@��H@��@�@�@��@�ݘ@���@��7@�"�@��6@�#:@��{@���@�_@��:@���@�,=@��6@���@�|�@��p@�=q@���@�� @���@��@��;@��C@��@���@�{�@�l�@�D�@�GE@�2�@��@��@��#@��w@��$@���@�m]@�T�@�҉@��b@���@�u�@��@�Z�@���@�-@���@�7L@��@�-�@�b@���@��@��@���@�Ov@�E�@��r@��[@���@���@��e@�I�@��@��m@��V@�t�@�W?@�:�@�#�@���@���@�3�@�ݘ@���@�j@���@���@�H�@��@��@���@���@�w2@�a@�B�@��@��@�R�@��@��6@�~�@�H�@���@�q�@�=q@���@�b�@�6z@��@�4n@���@���@��P@��@��R@�a|@��n@�)_@�?}@�_p@�B�@�R�@�خ@���@��H@�tT@�q�@��@�PH@�:�@��@�  @�� @��:@�iD@�G�@�&�@���@��}@�oi@�H@�@��C@�W?@��X@��D@�M�@�b@���@�rG@�(�@�
=@���@���@�R�@�ݘ@��'@�N<@�#�@�@@���@���@�]d@�-@�7@��]@���@��@��q@�s@��]@��@�I�@���@��V@��7@�~�@�\�@�@��@���@�xl@�@���@�ݘ@�ݘ@��Q@��}@���@�?}@�q@���@��x@�:�@�x�@�A�@�S@��<@�a|@�-�@��T@��@@�x@�dZ@�\�@�O�@�A @���@���@��@��x@�u�@�	@���@���@�|�@�[W@�"�@�%@��@�Ft@���@���@�|@�H�@���@��?@�� @�h�@�K^@�3�@�u@���@��@���@�L�@�(@���@���@��,@���@��e@��@��@���@�Ta@�1@��
@��'@�o�@�
=@��v@��4@�4@�P@~��@~ff@~5?@~�@}�"@}}�@}|@}#�@|g8@{�;@{=@z��@z�@z5?@y+�@x�9@xoi@x�@w��@w�@v��@v�H@v�s@v�B@v��@vTa@u��@u��@u�@ttT@tN�@t�@sl�@rq�@rOv@rOv@rGE@r.�@rJ@q��@qrG@p��@p��@p[�@p2�@o�@o�@n�@nff@nH�@n@m�@m�M@ma�@m@l�.@l(�@k�[@k�@jz@j �@i�#@i�@i \@hی@h|�@g��@gS@f��@f��@fd�@f�@e�.@e��@e�H@ef�@e#�@d�P@dی@d�Y@c��@c�@c�P@cn/@cK�@cK�@b҉@b0U@a�C@arG@`�@`�@`tT@`-�@_�0@_(@^h
@]�>@]��@\�`@\�@\u�@\2�@[�@[;d@Z�m@Z}V@Z0U@Y��@Y�@X�@X�U@X-�@W�&@Wƨ@W{J@V�]@V��@V�\@Vz@Vv�@Vv�@Vp;@VH�@U�T@U��@U��@U�@T�4@T�u@Tz�@S��@S=@S�@R�H@R�@ROv@Q�>@Q�j@Q��@Q�@P��@O�@Os@O>�@N�8@N�y@N�B@N}V@N@�@M�S@L�`@L�4@L9X@K��@K��@Kj�@Kl�@J��@J��@J@�@J�@I�o@Izx@H�@H�.@H:�@H	�@G��@G�0@GX�@F�"@Fȴ@Fv�@E�)@E��@D��@D��@D�@DD�@C�}@C\)@CE9@C33@C�@B��@B�<@B��@B�A@B6�@Af�@A \@A�@@�@@�e@@`�@?�&@?x@>��@>~�@>�@=|@=X@=Dg@=-w@<֡@<g8@;��@;��@;v`@;g�@;�@:��@:0U@9�N@9��@9��@9[W@95�@9V@8�@8�4@8j@8�@7dZ@6�"@6��@6z@6�@5�j@5�@50�@4�@4~(@4D�@3�r@3a@2�@2�@2i�@2=q@1@1\�@1�@0�f@0֡@0��@0�I@0z�@/�@/��@/j�@.��@.z@.O@-�.@-�z@-Vm@-@-�@-%@,��@,�@+��@+�K@+�0@+�q@+K�@+�@*�@*��@*h
@*1�@)@) \@(ѷ@(��@(l"@(U2@(PH@(H@(Ft@(2�@'�W@'Mj@&��@&��@&i�@&e@%��@%N<@$��@$��@$�@$��@$~(@$l"@$*�@#�}@#iD@#/�@#@"�!@"1�@!��@!T�@!5�@!%@!%@ �@ ��@ e�@ 2�@�@��@n/@�@{�@��@�^@\�@8�@�@��@�z@N�@9X@"h@  @�@�g@��@t�@Z�@�@�@��@��@�1@n�@@�@�@�H@�@L�@�)@�@��@Z@%�@خ@��@4�@�"@��@s�@s�@GE@�@�=@��@p�@Y�@F@�K@ѷ@��@��@u�@"h@	�@��@˒@��@F�@�y@�@�@�R@��@� @YK@M�@C�@3�@#:@	@�Z@ԕ@��@��@j@B�@�@�@�@��@��@y>@tT@V�@>B@9X@,=@!@��@��@�Q@��@�0@��@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A֠'A�^�A�s�A�MA�LdAұ�A�M�A��#A���AН~A�s�Aς�A���A�'�A�'RAˏ�A��A�XA�VA�˒AɑhAɅ�A�aA�W�A�d�A�.IA�A��AȹXA�~(A�r�A�rA�J#A�AA��VA��A��A�ߤAǩ�AǇ�A�k�A�d&A�XEA�=A�A��AŮ}A�F�Aó�A���A��IA�Y�A��6A��dA���A��A��A�d�A��zA�V�A�cA�L�A�-�A�kA�JXA��A�LdA��A�}�A��A��NA���A��)A�4nA��WA�A A�A���A�[�A���A��"A}e,Aw��Ap�WAl�"Aj�Ag�kAbn/A`qA^b�A\TaA[��AZz�AY�AW�AWOvAU�AR�AP�6APqAL+�AH��AFAD�OACh
A?\)A=�A;��A:r�A7DgA4bNA4��A4OA3҉A2}�A/�VA.��A-PHA,��A,��A,��A,I�A++A*<6A)��A)v`A(OvA'S&A&�A'U�A'9XA&
�A$��A!�\A2aAqA�uA�DA��A2aA�A-A&AQ�AA A0�A�A�A�jA��A}�An�AVAE�A�Am]A,=A�.A�,A�VAVA{�A�AffAC�A�KA�fA-�A�1A \A��A\)A�xAP�A(A�AA��Ac�A�A�6Ap�A��A
�zA	��A�A�TAe�A�9A��A
�A��A@�A%�A�TA��A�YA��A]dA]dA֡AA5�AE9A��Ae�AJA c A �@��W@��@��@�!�@���@���@��0@�H@�ƨ@�e,@��@�خ@��t@���@� \@��F@�7@��g@���@���@��"@�,�@�+@�&�@��@�h@��@�C@�S@�ی@�"h@�q@��@�}�@�"�@��@��@�*�@��K@�N<@ꀝ@�$@�{@�@@��@��c@� @�w�@�G@�s@��[@䤩@�:*@��9@��@�S@�[W@��8@�	@��2@�-�@��@�O@�H@�g�@��@ޗ�@�9X@ݹ�@�K�@��f@ܶ�@�s�@ۚk@�ѷ@��@ن�@��@�~(@��@ץ�@�S&@ֆY@�b@��D@�ƨ@�7L@��'@Ԁ�@��@�H�@�J�@��@��@у{@��8@��Z@�ں@�@�o�@���@̚�@�:*@���@�o�@ʌ�@�($@��#@ə�@�>�@�E9@���@�c�@�1'@��T@�+�@Ʈ}@���@���@���@Ļ�@�H@Úk@�҉@�9X@��>@�>�@��@�Ĝ@�z@�-�@��3@�n/@�H�@�%F@��H@��@�@�@��@�ݘ@���@��7@�"�@��6@�#:@��{@���@�_@��:@���@�,=@��6@���@�|�@��p@�=q@���@�� @���@��@��;@��C@��@���@�{�@�l�@�D�@�GE@�2�@��@��@��#@��w@��$@���@�m]@�T�@�҉@��b@���@�u�@��@�Z�@���@�-@���@�7L@��@�-�@�b@���@��@��@���@�Ov@�E�@��r@��[@���@���@��e@�I�@��@��m@��V@�t�@�W?@�:�@�#�@���@���@�3�@�ݘ@���@�j@���@���@�H�@��@��@���@���@�w2@�a@�B�@��@��@�R�@��@��6@�~�@�H�@���@�q�@�=q@���@�b�@�6z@��@�4n@���@���@��P@��@��R@�a|@��n@�)_@�?}@�_p@�B�@�R�@�خ@���@��H@�tT@�q�@��@�PH@�:�@��@�  @�� @��:@�iD@�G�@�&�@���@��}@�oi@�H@�@��C@�W?@��X@��D@�M�@�b@���@�rG@�(�@�
=@���@���@�R�@�ݘ@��'@�N<@�#�@�@@���@���@�]d@�-@�7@��]@���@��@��q@�s@��]@��@�I�@���@��V@��7@�~�@�\�@�@��@���@�xl@�@���@�ݘ@�ݘ@��Q@��}@���@�?}@�q@���@��x@�:�@�x�@�A�@�S@��<@�a|@�-�@��T@��@@�x@�dZ@�\�@�O�@�A @���@���@��@��x@�u�@�	@���@���@�|�@�[W@�"�@�%@��@�Ft@���@���@�|@�H�@���@��?@�� @�h�@�K^@�3�@�u@���@��@���@�L�@�(@���@���@��,@���@��e@��@��@���@�Ta@�1@��
@��'@�o�@�
=@��v@��4@�4@�P@~��@~ff@~5?@~�@}�"@}}�@}|@}#�@|g8@{�;@{=@z��@z�@z5?@y+�@x�9@xoi@x�@w��@w�@v��@v�H@v�s@v�B@v��@vTa@u��@u��@u�@ttT@tN�@t�@sl�@rq�@rOv@rOv@rGE@r.�@rJ@q��@qrG@p��@p��@p[�@p2�@o�@o�@n�@nff@nH�@n@m�@m�M@ma�@m@l�.@l(�@k�[@k�@jz@j �@i�#@i�@i \@hی@h|�@g��@gS@f��@f��@fd�@f�@e�.@e��@e�H@ef�@e#�@d�P@dی@d�Y@c��@c�@c�P@cn/@cK�@cK�@b҉@b0U@a�C@arG@`�@`�@`tT@`-�@_�0@_(@^h
@]�>@]��@\�`@\�@\u�@\2�@[�@[;d@Z�m@Z}V@Z0U@Y��@Y�@X�@X�U@X-�@W�&@Wƨ@W{J@V�]@V��@V�\@Vz@Vv�@Vv�@Vp;@VH�@U�T@U��@U��@U�@T�4@T�u@Tz�@S��@S=@S�@R�H@R�@ROv@Q�>@Q�j@Q��@Q�@P��@O�@Os@O>�@N�8@N�y@N�B@N}V@N@�@M�S@L�`@L�4@L9X@K��@K��@Kj�@Kl�@J��@J��@J@�@J�@I�o@Izx@H�@H�.@H:�@H	�@G��@G�0@GX�@F�"@Fȴ@Fv�@E�)@E��@D��@D��@D�@DD�@C�}@C\)@CE9@C33@C�@B��@B�<@B��@B�A@B6�@Af�@A \@A�@@�@@�e@@`�@?�&@?x@>��@>~�@>�@=|@=X@=Dg@=-w@<֡@<g8@;��@;��@;v`@;g�@;�@:��@:0U@9�N@9��@9��@9[W@95�@9V@8�@8�4@8j@8�@7dZ@6�"@6��@6z@6�@5�j@5�@50�@4�@4~(@4D�@3�r@3a@2�@2�@2i�@2=q@1@1\�@1�@0�f@0֡@0��@0�I@0z�@/�@/��@/j�@.��@.z@.O@-�.@-�z@-Vm@-@-�@-%@,��@,�@+��@+�K@+�0@+�q@+K�@+�@*�@*��@*h
@*1�@)@) \@(ѷ@(��@(l"@(U2@(PH@(H@(Ft@(2�@'�W@'Mj@&��@&��@&i�@&e@%��@%N<@$��@$��@$�@$��@$~(@$l"@$*�@#�}@#iD@#/�@#@"�!@"1�@!��@!T�@!5�@!%@!%@ �@ ��@ e�@ 2�@�@��@n/@�@{�@��@�^@\�@8�@�@��@�z@N�@9X@"h@  @�@�g@��@t�@Z�@�@�@��@��@�1@n�@@�@�@�H@�@L�@�)@�@��@Z@%�@خ@��@4�@�"@��@s�@s�@GE@�@�=@��@p�@Y�@F@�K@ѷ@��@��@u�@"h@	�@��@˒@��@F�@�y@�@�@�R@��@� @YK@M�@C�@3�@#:@	@�Z@ԕ@��@��@j@B�@�@�@�@��@��@y>@tT@V�@>B@9X@,=@!@��@��@�Q@��@�0@��@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   Br�Bu?BpUBq�BoOBn/BlBmCBqABp�BnIBUMB.BWB	7B �B�6B��B��B
	BB �B"�B(�BBuBN�BP.B_BxRB�(B�NB��B��B�vB��B�]B�B��B��B��B��B��B�B�YB�lB	4nB	lqB	�9B
>B	�6B
�yB
�vB �B  B
��B
��B
�B
�B
�B
�B
�B
өB
�"B
��B
��B
��B
g�B
M�B
1�B
�B	��B
�B
t�B
lqB
f�B
?HB	�B	�TB
	7B

�B	�B	�	B	�B	tnB	c:B	VB	J�B	:�B	1B	&�B	�B	qB	�B	BB		�B	�B��B�B�B��B�B�:B�B��B	�B	)B	�B�B��B��B	�B	%�B	<�B	AoB	J#B	E�B	K�B	S�B	V�B	X�B	o�B	}�B	�B	��B	��B	��B	��B	��B	�=B	�oB	�B	�FB	�&B	{�B	k�B	h�B	d�B	f�B	i_B	m]B	r�B	xB	~(B	��B	��B	��B	�YB	�_B	�B	�B	�1B	�B	��B	��B	�YB	�EB	��B	��B	�?B	��B	��B	��B	|PB	�B	��B	�~B	�<B	��B	��B	�KB	��B	�>B	�B	�zB	��B	�B	�,B	��B	��B	�FB	�zB	�B	��B	�>B	��B	�0B	��B	�B	��B	��B	��B	��B	�B	�mB	�
B	��B	��B	��B	�B	�!B	��B	�B	�B	�B	�#B	ȚB	��B	��B	�	B	��B	�XB	ǮB	ǔB	��B	��B	ǮB	ƨB	�+B	�%B	��B	��B	�B	�?B	ňB	ƎB	ƨB	��B	ƎB	�zB	�7B	��B	�DB	��B	�"B	�NB	��B	ѝB	ԕB	��B	�[B	��B	�YB	��B	یB	�	B	چB	ܬB	�;B	ߤB	�xB	��B	ӏB	�uB	ӏB	�FB	�B	��B	�SB	�sB	רB	�yB	�1B	�1B	�KB	ٚB	��B	ٚB	�B	�B	��B	��B	�bB	�B	�B	�B	��B	��B	��B	�B	�HB	�hB	�'B	�jB	�B	�5B	�B	�B	�B	ޞB	��B	��B	�pB	޸B	�!B	��B	��B	��B	�B	�B	�B	�ZB	��B	��B	�B	�B	� B	��B	�B	�TB	�:B	�B	�tB	��B	�B	��B	�B	��B	��B	�B	��B	�$B	�
B	��B	�B	�KB	�"B	��B	��B	�)B	��B	�=B	�B	�=B	�B	�)B	�)B	�)B	�wB	�wB	��B	�wB	��B	�IB	�B	�B	�B	�B	�;B	�B	�B	�'B	�B	��B	�9B	�?B	��B	��B	��B	�MB	�MB	�B	�B	�B	��B	�ZB	��B	��B	��B	�XB	�DB	�B	��B	��B	��B	�B	�PB	��B	��B	��B	�<B	�<B	�B	��B
B
UB
 B
UB
�B
�B
uB
-B
�B
�B
{B
�B
 �B
 �B
B
3B
�B
�B
SB
mB
�B
B
fB
	B
�B
	B
	�B
	lB
	RB
	B
�B
�B
�B
�B
�B

XB
�B
�B
�B
pB
�B
�B
\B
BB
\B
�B
�B
.B
HB
�B
hB
�B
�B
TB
oB
oB
:B
�B
�B
�B
&B
B
�B
TB
oB
TB
 B
 B
hB
�B
bB
B
BB
NB
B
�B
 B
�B
bB
TB
4B
B
:B
B
�B
�B
uB
uB
&B
[B
[B
[B
�B
B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 B
 B
�B
B
�B
�B
uB
�B
�B
�B
FB
FB
�B
�B
�B
�B
B
B
2B
�B
�B
_B
yB
+B
B
KB
KB
�B
WB
�B
�B
�B
#B
WB
qB
�B
�B
�B
CB
IB
/B
�B
OB
�B
jB
�B
�B
�B
pB
�B
�B
 \B
 �B
 �B
 �B
 �B
 �B
!bB
!�B
"4B
"�B
"�B
#nB
#�B
#�B
#�B
#�B
$B
#�B
$@B
%B
$�B
$�B
%B
%�B
%�B
%�B
&LB
&�B
&�B
&�B
&�B
'mB
'�B
)B
)�B
)�B
)�B
*B
*0B
*0B
*KB
*KB
*eB
*eB
*B
+B
+6B
+�B
+�B
,=B
,WB
,�B
-wB
-�B
.�B
.�B
/ B
/B
/�B
/iB
/iB
/�B
0;B
0�B
1'B
1AB
1'B
1�B
2�B
2�B
2�B
2�B
3�B
49B
49B
49B
49B
4B
49B
4�B
4�B
5ZB
6`B
6�B
72B
7fB
7�B
88B
8lB
8�B
8�B
8lB
8B
8B
8�B
9>B
9XB
9$B
9	B
9XB
9rB
9rB
9�B
9�B
9�B
9�B
9�B
9�B
9�B
:DB
:�B
:�B
;dB
<6B
<jB
<�B
<�B
=VB
=�B
>B
>�B
?}B
?�B
?�B
?�B
?�B
?�B
?�B
@ B
@OB
@OB
@iB
@�B
@�B
AoB
AoB
AUB
AoB
A�B
AoB
A;B
@�B
A;B
AUB
AUB
@�B
@�B
@�B
AB
A;B
A�B
A�B
B'B
B�B
B�B
CB
C-B
CGB
C�B
C�B
C�B
C�B
DMB
D�B
D�B
D�B
EB
EB
EB
E9B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
FB
FB
F%B
F�B
F�B
F�B
F�B
G�B
HKB
HfB
H�B
H�B
I7B
I�B
I�B
J	B
J�B
J�B
K�B
K�B
LB
LJB
LJB
LJB
L�B
L�B
M6B
M�B
M�B
NB
N"B
N�B
NpB
N<B
N�B
N�B
O(B
OBB
OBB
O�B
PB
P.B
PbB
P�B
P}B
P�B
Q B
Q4B
Q4B
Q�B
Q�B
RB
R�B
R�B
R�B
R�B
S&B
SuB
S�B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
T�B
T�B
T�B
T�B
UB
UMB
U�B
U�B
VSB
V�B
V�B
WsB
WsB
W�B
WsB
W�B
XB
X�B
X�B
X�B
X�B
X�B
YB
Y�B
Y�B
Y�B
Y�B
ZB
Z7B
Z7B
ZkB
Z�B
Z�B
Z�B
[qB
[�B
[�B
[�B
\CB
\xB
\�B
\�B
\�B
]IB
]~B
]�B
^B
^jB
^�B
^�B
^�B
_B
_VB
_�B
_�B
_�B
_�B
_�B
_�B
`vB
`�B
`vB
a-B
abB
a�B
a�B
a�B
bhB
b�B
bhB
b�B
cB
c B
c:B
cnB
c�B
c�B
c�B
c�B
dB
d�B
e,B
e`B
e�B
eFB
ezB
e�B
e�B
e�B
e�B
fB
e�B
f2B
f2B
f�B
gB
gB
gmB
gmB
g�B
hXB
h�B
h�B
h�B
h�B
h�B
i�B
i�B
j0B
j�B
j�B
j�B
kB
kQB
k�B
l"B
l"B
lWB
lWB
lqB
l�B
l�B
l�B
m)B
m)B
mwB
m�B
m�B
m�B
n/B
n�B
n�B
n�B
o B
oB
o�B
o�B
pB
pUB
p�B
p�B
p�B
p�B
p�B
qAB
q[B
q�B
q�B
q�B
q�B
q�B
rB
r|B
raB
r�B
s3B
shB
s�B
s�B
tB
tB
tnB
t�B
t�B
t�B
t�B
t�B
uB
utB
u�B
u�B
u�B
u�B
u�B
vFB
v+B
vFB
v`B
v�B
v�B
v�B
wB
wB
w2B
w�B
x8B
xB
x8B
x8B
xlB
xlB
x�B
x�B
x�B
x�B
x�B
y>B
y$B
y>B
yrB
y�B
y�B
y�B
z*B
zB
z*B
zxB
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
{B
{B
{B
{0B
{dB
{J111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   Br�Bu?BpUBq�BoOBn/BlBmCBqABp�BnIBUMB.BWB	7B �B�6B��B��B
	BB �B"�B(�BBuBN�BP.B_BxRB�(B�NB��B��B�vB��B�]B�B��B��B��B��B��B�B�YB�lB	4nB	lqB	�9B
>B	�6B
�yB
�vB �B  B
��B
��B
�B
�B
�B
�B
�B
өB
�"B
��B
��B
��B
g�B
M�B
1�B
�B	��B
�B
t�B
lqB
f�B
?HB	�B	�TB
	7B

�B	�B	�	B	�B	tnB	c:B	VB	J�B	:�B	1B	&�B	�B	qB	�B	BB		�B	�B��B�B�B��B�B�:B�B��B	�B	)B	�B�B��B��B	�B	%�B	<�B	AoB	J#B	E�B	K�B	S�B	V�B	X�B	o�B	}�B	�B	��B	��B	��B	��B	��B	�=B	�oB	�B	�FB	�&B	{�B	k�B	h�B	d�B	f�B	i_B	m]B	r�B	xB	~(B	��B	��B	��B	�YB	�_B	�B	�B	�1B	�B	��B	��B	�YB	�EB	��B	��B	�?B	��B	��B	��B	|PB	�B	��B	�~B	�<B	��B	��B	�KB	��B	�>B	�B	�zB	��B	�B	�,B	��B	��B	�FB	�zB	�B	��B	�>B	��B	�0B	��B	�B	��B	��B	��B	��B	�B	�mB	�
B	��B	��B	��B	�B	�!B	��B	�B	�B	�B	�#B	ȚB	��B	��B	�	B	��B	�XB	ǮB	ǔB	��B	��B	ǮB	ƨB	�+B	�%B	��B	��B	�B	�?B	ňB	ƎB	ƨB	��B	ƎB	�zB	�7B	��B	�DB	��B	�"B	�NB	��B	ѝB	ԕB	��B	�[B	��B	�YB	��B	یB	�	B	چB	ܬB	�;B	ߤB	�xB	��B	ӏB	�uB	ӏB	�FB	�B	��B	�SB	�sB	רB	�yB	�1B	�1B	�KB	ٚB	��B	ٚB	�B	�B	��B	��B	�bB	�B	�B	�B	��B	��B	��B	�B	�HB	�hB	�'B	�jB	�B	�5B	�B	�B	�B	ޞB	��B	��B	�pB	޸B	�!B	��B	��B	��B	�B	�B	�B	�ZB	��B	��B	�B	�B	� B	��B	�B	�TB	�:B	�B	�tB	��B	�B	��B	�B	��B	��B	�B	��B	�$B	�
B	��B	�B	�KB	�"B	��B	��B	�)B	��B	�=B	�B	�=B	�B	�)B	�)B	�)B	�wB	�wB	��B	�wB	��B	�IB	�B	�B	�B	�B	�;B	�B	�B	�'B	�B	��B	�9B	�?B	��B	��B	��B	�MB	�MB	�B	�B	�B	��B	�ZB	��B	��B	��B	�XB	�DB	�B	��B	��B	��B	�B	�PB	��B	��B	��B	�<B	�<B	�B	��B
B
UB
 B
UB
�B
�B
uB
-B
�B
�B
{B
�B
 �B
 �B
B
3B
�B
�B
SB
mB
�B
B
fB
	B
�B
	B
	�B
	lB
	RB
	B
�B
�B
�B
�B
�B

XB
�B
�B
�B
pB
�B
�B
\B
BB
\B
�B
�B
.B
HB
�B
hB
�B
�B
TB
oB
oB
:B
�B
�B
�B
&B
B
�B
TB
oB
TB
 B
 B
hB
�B
bB
B
BB
NB
B
�B
 B
�B
bB
TB
4B
B
:B
B
�B
�B
uB
uB
&B
[B
[B
[B
�B
B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 B
 B
�B
B
�B
�B
uB
�B
�B
�B
FB
FB
�B
�B
�B
�B
B
B
2B
�B
�B
_B
yB
+B
B
KB
KB
�B
WB
�B
�B
�B
#B
WB
qB
�B
�B
�B
CB
IB
/B
�B
OB
�B
jB
�B
�B
�B
pB
�B
�B
 \B
 �B
 �B
 �B
 �B
 �B
!bB
!�B
"4B
"�B
"�B
#nB
#�B
#�B
#�B
#�B
$B
#�B
$@B
%B
$�B
$�B
%B
%�B
%�B
%�B
&LB
&�B
&�B
&�B
&�B
'mB
'�B
)B
)�B
)�B
)�B
*B
*0B
*0B
*KB
*KB
*eB
*eB
*B
+B
+6B
+�B
+�B
,=B
,WB
,�B
-wB
-�B
.�B
.�B
/ B
/B
/�B
/iB
/iB
/�B
0;B
0�B
1'B
1AB
1'B
1�B
2�B
2�B
2�B
2�B
3�B
49B
49B
49B
49B
4B
49B
4�B
4�B
5ZB
6`B
6�B
72B
7fB
7�B
88B
8lB
8�B
8�B
8lB
8B
8B
8�B
9>B
9XB
9$B
9	B
9XB
9rB
9rB
9�B
9�B
9�B
9�B
9�B
9�B
9�B
:DB
:�B
:�B
;dB
<6B
<jB
<�B
<�B
=VB
=�B
>B
>�B
?}B
?�B
?�B
?�B
?�B
?�B
?�B
@ B
@OB
@OB
@iB
@�B
@�B
AoB
AoB
AUB
AoB
A�B
AoB
A;B
@�B
A;B
AUB
AUB
@�B
@�B
@�B
AB
A;B
A�B
A�B
B'B
B�B
B�B
CB
C-B
CGB
C�B
C�B
C�B
C�B
DMB
D�B
D�B
D�B
EB
EB
EB
E9B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
FB
FB
F%B
F�B
F�B
F�B
F�B
G�B
HKB
HfB
H�B
H�B
I7B
I�B
I�B
J	B
J�B
J�B
K�B
K�B
LB
LJB
LJB
LJB
L�B
L�B
M6B
M�B
M�B
NB
N"B
N�B
NpB
N<B
N�B
N�B
O(B
OBB
OBB
O�B
PB
P.B
PbB
P�B
P}B
P�B
Q B
Q4B
Q4B
Q�B
Q�B
RB
R�B
R�B
R�B
R�B
S&B
SuB
S�B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
T�B
T�B
T�B
T�B
UB
UMB
U�B
U�B
VSB
V�B
V�B
WsB
WsB
W�B
WsB
W�B
XB
X�B
X�B
X�B
X�B
X�B
YB
Y�B
Y�B
Y�B
Y�B
ZB
Z7B
Z7B
ZkB
Z�B
Z�B
Z�B
[qB
[�B
[�B
[�B
\CB
\xB
\�B
\�B
\�B
]IB
]~B
]�B
^B
^jB
^�B
^�B
^�B
_B
_VB
_�B
_�B
_�B
_�B
_�B
_�B
`vB
`�B
`vB
a-B
abB
a�B
a�B
a�B
bhB
b�B
bhB
b�B
cB
c B
c:B
cnB
c�B
c�B
c�B
c�B
dB
d�B
e,B
e`B
e�B
eFB
ezB
e�B
e�B
e�B
e�B
fB
e�B
f2B
f2B
f�B
gB
gB
gmB
gmB
g�B
hXB
h�B
h�B
h�B
h�B
h�B
i�B
i�B
j0B
j�B
j�B
j�B
kB
kQB
k�B
l"B
l"B
lWB
lWB
lqB
l�B
l�B
l�B
m)B
m)B
mwB
m�B
m�B
m�B
n/B
n�B
n�B
n�B
o B
oB
o�B
o�B
pB
pUB
p�B
p�B
p�B
p�B
p�B
qAB
q[B
q�B
q�B
q�B
q�B
q�B
rB
r|B
raB
r�B
s3B
shB
s�B
s�B
tB
tB
tnB
t�B
t�B
t�B
t�B
t�B
uB
utB
u�B
u�B
u�B
u�B
u�B
vFB
v+B
vFB
v`B
v�B
v�B
v�B
wB
wB
w2B
w�B
x8B
xB
x8B
x8B
xlB
xlB
x�B
x�B
x�B
x�B
x�B
y>B
y$B
y>B
yrB
y�B
y�B
y�B
z*B
zB
z*B
zxB
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
{B
{B
{B
{0B
{dB
{J111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604105236  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604191813  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604191813  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604191813                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605041820  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605041820  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610151508                      G�O�G�O�G�O�                