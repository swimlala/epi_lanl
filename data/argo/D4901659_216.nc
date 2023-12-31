CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2020-06-28T22:00:59Z creation      
references        (http://www.argodatamgt.org/Documentation   comment           user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7(   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7h   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     7�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8,   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�E�vQ�   
_FillValue        A.�~       axis      T           80   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    88   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�E�vQ�   
_FillValue        A.�~            8<   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8D   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8L   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8T   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8X   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8`   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9`   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9d   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9h   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9l   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  `�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  p@   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �|   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �h   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �L   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �8   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �<   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �@   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �D   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �H   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �L   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �        �Argo profile    3.1 1.2 19500101000000  20200628220059  20230721230927  4901659 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               �A   AO  5382                            2C  D   NAVIS_A                         0371                            082713                          863 @�$���&1   @�$��[`@<o��-V�c�7Kƨ1   GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                      �A   A   A   @���@�  A��AffA@  A`  A�  A�  A�  A�33A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӃ3D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�
>@�p�A Q�A�A>�RA^�RA~�RA�\)A�\)A��\A�\)A�\)A�\)A�\)A�\)B�B�B�B�B'�B/�B7�B?�BG�BO�BW�B_�Bg�Bo�Bw�B�B��
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
C�C�C�C�C	�C�C�C�C�C�C�C�C�C�C�C�C!�C#�C%�C'�C)�C+�C-�C/�C1�C3�C5�C7�C9�C;�C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D z�D ��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��D	z�D	��D
z�D
��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D�{Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��D z�D ��D!z�D!��D"z�D"��D#z�D#��D$z�D$��D%z�D%��D&z�D&��D'z�D'��D(z�D(��D)z�D)��D*z�D*��D+z�D+��D,z�D,��D-z�D-��D.z�D.��D/z�D/��D0z�D0��D1z�D1��D2z�D2��D3z�D3��D4z�D4��D5z�D5��D6z�D6��D7z�D7��D8z�D8��D9z�D9��D:z�D:��D;z�D;��D<z�D<��D=z�D=��D>z�D>��D?z�D?��D@z�D@��DAz�DA��DBz�DB��DCz�DC��DDz�DD��DEz�DE��DFz�DF��DGz�DG��DHz�DH��DIz�DI��DJz�DJ��DKz�DK��DLz�DL��DMz�DM��DNz�DN��DOz�DO��DPz�DP��DQz�DQ��DRz�DR��DSz�DS��DTz�DT��DUz�DU��DVz�DV��DWz�DW��DXz�DX��DYz�DY��DZz�DZ��D[z�D[��D\z�D\��D]z�D]��D^z�D^��D_z�D_��D`z�D`��Daz�Da��Dbz�Db��Dcz�Dc��Ddz�Dd��Dez�De��Dfz�Df��Dgz�Dg��Dhz�Dh��Diz�Di��Djz�Dj��Dkz�Dk��Dlz�Dl��Dmz�Dm��Dnz�Dn��Doz�Do��Dpz�Dp��Dqz�Dq��Drz�Dr��Dsz�Ds��Dtz�Dt��Duz�Du��Dvz�Dv��Dwz�Dw��Dxz�Dx��Dyz�Dy��Dzz�Dz��D{z�D{��D|z�D|��D}z�D}��D~z�D~��Dz�D��D�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�@�D�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�z>D��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD½qD��qD�=qD�}qDýqD��qD�=qD�}qDĽqD��qD�=qD�}qDŽqD��qD�=qD�}qDƽqD��qD�=qD�}qDǽqD��qD�=qD�}qDȽqD��qD�=qD�}qDɽqD��qD�=qD�}qDʽqD��qD�=qD�}qD˽qD��qD�=qD�}qD̽qD��qD�=qD�}qDͽqD��qD�=qD�}qDνqD��qD�=qD�}qDϽqD��qD�=qD�}qDнqD��qD�=qD�}qDѽqD��qD�=qD�}qDҽqD��qD�=qDӀ�DӽqD��qD�=qD�}qDԽqD��qD�=qD�}qDսqD��qD�=qD�}qDֽqD��qD�=qD�}qD׽qD��qD�=qD�}qDؽqD��qD�=qD�}qDٽqD��qD�=qD�}qDڽqD��qD�=qD�}qD۽qD��qD�=qD�}qDܽqD��qD�=qD�}qDݽqD��qD�=qD�}qD޽qD��qD�=qD�}qD߽qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD��qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD���D��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD���D��>111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�|�A�r�A�t�A�r�AсAуA�~�AсAхAщ7AхA�t�A�ffA�bA̕�A�r�A�l�A�jA�33A��AǑhA�G�A��A�O�A���AđhA�dZA�hsA�jA��/A�C�A��;A���A���A�hsA�K�A��mA��PA��;A�G�A��uA��A���A�;dA��A��A��A�ZA���A���A��A��A��A�A�C�A�1'A��A�;dA��A�/A�
=A��A�33A�ȴA�M�A��-A��+A�l�A���A��A��A���A�x�A��HA�33A�VA��`A�C�A��TA�p�A��A�1'A�jA���A���A�-A�"�A��A�Q�A�\)A�33A���A��A�G�A�\)A�5?A�7LA��uA�v�A��TA��\A��A���A�K�A�{A���A�K�A�bNA7LA}�hAz�Ayp�AxffAw;dAv�+Aul�As�
Ar9XAo�Al�yAk��Aj�Ah��Ag��Af��Aet�Ad��Acx�Aa�A`ȴA_�FA^1A\�DAX��AV�AUXAT9XAS�wAQ%AO�7ANffAMO�AL�AJ�jAJM�AI�TAIXAH��AG��AEVAC�AB�AB�jAB��AB�+ABE�AAC�A@��A?�hA>��A>=qA=XA<��A<JA;+A:1'A9�;A9;dA7��A6n�A5�A3�;A3�A2��A0r�A/?}A.�HA.�!A.ZA-t�A,��A+�-A*��A*�A)��A)
=A(ȴA(�A'/A&�9A&jA&1A%�A$Q�A#�^A#oA"�A"bA!C�A ��A �A��A  A��AhsA�\A��At�A�A��AZAJA��Ax�A�A^5A�
A$�A  A�mA�^Ap�AC�A33A��A�RA�DAI�AG�A�AVAjAG�A�A�\A�A��A
ȴA
bNA
E�A
bAbNAbNA�A��A��A �AG�A �j@���@�^5@��@�
=@��@�X@��@��@���@��#@�7L@��`@���@�D@� �@�;d@��T@�@��@�K�@@��@�/@���@���@�@�ȴ@��/@䛦@�9X@�@�  @ޗ�@�S�@�-@ٺ^@�X@ش9@׍P@թ�@�Ĝ@��m@�M�@���@�"�@�ȴ@·+@�M�@��@�~�@�@ɺ^@ə�@�7L@�33@�$�@ź^@��@Å@�ff@�O�@�A�@��;@��F@�t�@���@�V@�@�p�@�Ĝ@��P@�-@�j@���@�C�@�
=@�M�@�O�@��@�I�@�S�@�ff@���@�9X@�l�@��+@�$�@���@�?}@��`@�Z@��@�S�@�@�=q@�hs@��@�A�@���@�
=@�V@�E�@�M�@�=q@��@���@��!@�^5@�M�@�-@�$�@��@��@��h@���@�1@��P@�33@�o@��@���@�@��^@��@�`B@�&�@�Ĝ@�j@� �@���@��H@���@��`@�Q�@���@��H@�~�@�M�@�{@�@���@��-@�p�@�V@��D@�1@��;@���@�33@��\@�@���@�x�@�?}@�%@���@���@��@�Q�@���@���@�E�@���@�G�@�/@�V@�Ĝ@�j@�1@��P@�"�@��y@���@�ff@�J@�@���@��@�?}@���@�r�@�I�@� �@��@��
@��P@�;d@��@��@�v�@�$�@��T@��^@���@��7@�hs@��@���@��@�bN@�1'@���@���@���@��@�S�@�@�@���@��H@��\@�=q@�$�@��@�{@���@��@�hs@�?}@��@�Ĝ@�1'@�b@�@l�@+@
=@~�y@~ȴ@~E�@~$�@}V@|9X@{��@{�F@{�@{@z��@z=q@zJ@y�^@y��@yhs@x��@x1'@w|�@w�@vV@u�-@u/@uV@t�/@t9X@s��@s�F@s��@s��@s�@st�@sS�@sC�@s33@so@r�@r�!@r^5@q�#@q�7@q��@q��@q�7@qX@pQ�@pb@o��@o\)@o\)@o;d@n�@n�+@n@m��@m?}@mV@l�@l�@l�/@l�@l(�@kt�@ko@j�\@i�@h �@h  @g��@g�w@g�@g�@fȴ@fȴ@f�@f�@f��@f�+@e�@e�h@e`B@d��@d�j@d�@d�D@dZ@d9X@d1@cC�@bM�@a�#@a��@ax�@a��@a��@aX@`��@`Ĝ@`Q�@_�P@_l�@_;d@_�@_
=@_
=@^��@^ȴ@^��@]�@]`B@]�@\�@\��@[dZ@Z��@Z�\@Z�\@Zn�@Y��@YX@Y7L@Y�@X��@X��@XQ�@W�;@W��@Wl�@W\)@WK�@W+@W
=@V�y@Vȴ@V��@Vff@V$�@U�@UV@T�@Tj@T(�@S�
@Sƨ@S��@S��@S�F@SS�@S33@R~�@Q��@Qx�@Q&�@PQ�@O��@O�w@O�P@O+@N�@Nȴ@N��@N�+@Nff@NV@NE�@NE�@N@M�h@M/@L��@L�/@L�D@L1@K�
@K�F@K�@K"�@J^5@J-@JJ@I�@I��@I&�@I�@H��@HĜ@H��@HbN@H1'@H �@H �@H �@G��@G;d@F��@F�R@F�+@FV@F@E�T@E?}@D�@Dj@DI�@C�
@Ct�@B�!@B^5@B=q@BJ@A�#@A�^@A�7@AX@A7L@@�`@@A�@@1'@@ �@@b@@b@@b@@b@?\)@>�y@>�R@>@=��@=p�@=�@<��@<�@<�/@<�@<j@<9X@<1@;��@;�
@;ƨ@;�F@;��@;�@;t�@;C�@;@:�H@:�!@:~�@:-@9��@9�#@9��@9X@8��@8 �@7�;@7��@7�P@7l�@7+@6ȴ@6ff@65?@5�@5��@5O�@5�@4�/@4�j@4�D@4j@4j@4Z@49X@41@3ƨ@3�@3S�@333@3o@2�@2��@2^5@2=q@1�#@1�7@1x�@1�@0��@0��@0r�@0Q�@01'@0 �@0  @/�w@/�P@/K�@/K�@/+@.�y@.�y@.�R@.�R@.�+@.E�@.@-�-@-`B@,��@,�D@,(�@,1@+ƨ@+dZ@+o@*��@*�\@*=q@)��@)G�@(Ĝ@(r�@(1'@(b@'�w@&��@&��@&v�@&E�@&{@%@%�h@%`B@%?}@%V@$�/@$Z@$(�@#�
@#�F@#��@#��@#��@#��@#�@#t�@#33@#@"~�@"M�@!�#@!�^@!X@!%@ ��@ ��@ Ĝ@ Ĝ@ �9@ �@ r�@ bN@ A�@  �@   @   @�@|�@K�@
=@�y@�R@ff@5?@$�@�@��@�-@`B@O�@�@��@�/@�j@�D@j@I�@(�@��@ƨ@��@dZ@@�!@�\@~�@n�@^5@�@��@x�@hs@7L@%@��@�`@�`@�9@1'@�@��@�w@��@��@��@��@��@�P@\)@+@
=@ȴ@�+@E�@@�@�@��@p�@?}@�/@��@9X@��@C�@@��@^5@�@J@��@�#@�7@hs@��@�9@�9@��@�u@�u@�@r�@r�@A�@ �@��@��@l�@K�@;d@�@�@�R@E�@��@��@�@�@`B@�@�@�@��@�j@��@��@�D@�D@�D@�D@z�@I�@��@ƨ@��@t�@dZ@C�@o@
��@
�!@
�!@
�\@
n�@
=q@
J@	�@	��@	x�@	x�@	X@	G�@	7L@��@Ĝ@��@A�@�@�@�P@l�@+@��@�y@ȴ@�R@��@V@{@{@�@@p�@p�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�|�A�r�A�t�A�r�AсAуA�~�AсAхAщ7AхA�t�A�ffA�bA̕�A�r�A�l�A�jA�33A��AǑhA�G�A��A�O�A���AđhA�dZA�hsA�jA��/A�C�A��;A���A���A�hsA�K�A��mA��PA��;A�G�A��uA��A���A�;dA��A��A��A�ZA���A���A��A��A��A�A�C�A�1'A��A�;dA��A�/A�
=A��A�33A�ȴA�M�A��-A��+A�l�A���A��A��A���A�x�A��HA�33A�VA��`A�C�A��TA�p�A��A�1'A�jA���A���A�-A�"�A��A�Q�A�\)A�33A���A��A�G�A�\)A�5?A�7LA��uA�v�A��TA��\A��A���A�K�A�{A���A�K�A�bNA7LA}�hAz�Ayp�AxffAw;dAv�+Aul�As�
Ar9XAo�Al�yAk��Aj�Ah��Ag��Af��Aet�Ad��Acx�Aa�A`ȴA_�FA^1A\�DAX��AV�AUXAT9XAS�wAQ%AO�7ANffAMO�AL�AJ�jAJM�AI�TAIXAH��AG��AEVAC�AB�AB�jAB��AB�+ABE�AAC�A@��A?�hA>��A>=qA=XA<��A<JA;+A:1'A9�;A9;dA7��A6n�A5�A3�;A3�A2��A0r�A/?}A.�HA.�!A.ZA-t�A,��A+�-A*��A*�A)��A)
=A(ȴA(�A'/A&�9A&jA&1A%�A$Q�A#�^A#oA"�A"bA!C�A ��A �A��A  A��AhsA�\A��At�A�A��AZAJA��Ax�A�A^5A�
A$�A  A�mA�^Ap�AC�A33A��A�RA�DAI�AG�A�AVAjAG�A�A�\A�A��A
ȴA
bNA
E�A
bAbNAbNA�A��A��A �AG�A �j@���@�^5@��@�
=@��@�X@��@��@���@��#@�7L@��`@���@�D@� �@�;d@��T@�@��@�K�@@��@�/@���@���@�@�ȴ@��/@䛦@�9X@�@�  @ޗ�@�S�@�-@ٺ^@�X@ش9@׍P@թ�@�Ĝ@��m@�M�@���@�"�@�ȴ@·+@�M�@��@�~�@�@ɺ^@ə�@�7L@�33@�$�@ź^@��@Å@�ff@�O�@�A�@��;@��F@�t�@���@�V@�@�p�@�Ĝ@��P@�-@�j@���@�C�@�
=@�M�@�O�@��@�I�@�S�@�ff@���@�9X@�l�@��+@�$�@���@�?}@��`@�Z@��@�S�@�@�=q@�hs@��@�A�@���@�
=@�V@�E�@�M�@�=q@��@���@��!@�^5@�M�@�-@�$�@��@��@��h@���@�1@��P@�33@�o@��@���@�@��^@��@�`B@�&�@�Ĝ@�j@� �@���@��H@���@��`@�Q�@���@��H@�~�@�M�@�{@�@���@��-@�p�@�V@��D@�1@��;@���@�33@��\@�@���@�x�@�?}@�%@���@���@��@�Q�@���@���@�E�@���@�G�@�/@�V@�Ĝ@�j@�1@��P@�"�@��y@���@�ff@�J@�@���@��@�?}@���@�r�@�I�@� �@��@��
@��P@�;d@��@��@�v�@�$�@��T@��^@���@��7@�hs@��@���@��@�bN@�1'@���@���@���@��@�S�@�@�@���@��H@��\@�=q@�$�@��@�{@���@��@�hs@�?}@��@�Ĝ@�1'@�b@�@l�@+@
=@~�y@~ȴ@~E�@~$�@}V@|9X@{��@{�F@{�@{@z��@z=q@zJ@y�^@y��@yhs@x��@x1'@w|�@w�@vV@u�-@u/@uV@t�/@t9X@s��@s�F@s��@s��@s�@st�@sS�@sC�@s33@so@r�@r�!@r^5@q�#@q�7@q��@q��@q�7@qX@pQ�@pb@o��@o\)@o\)@o;d@n�@n�+@n@m��@m?}@mV@l�@l�@l�/@l�@l(�@kt�@ko@j�\@i�@h �@h  @g��@g�w@g�@g�@fȴ@fȴ@f�@f�@f��@f�+@e�@e�h@e`B@d��@d�j@d�@d�D@dZ@d9X@d1@cC�@bM�@a�#@a��@ax�@a��@a��@aX@`��@`Ĝ@`Q�@_�P@_l�@_;d@_�@_
=@_
=@^��@^ȴ@^��@]�@]`B@]�@\�@\��@[dZ@Z��@Z�\@Z�\@Zn�@Y��@YX@Y7L@Y�@X��@X��@XQ�@W�;@W��@Wl�@W\)@WK�@W+@W
=@V�y@Vȴ@V��@Vff@V$�@U�@UV@T�@Tj@T(�@S�
@Sƨ@S��@S��@S�F@SS�@S33@R~�@Q��@Qx�@Q&�@PQ�@O��@O�w@O�P@O+@N�@Nȴ@N��@N�+@Nff@NV@NE�@NE�@N@M�h@M/@L��@L�/@L�D@L1@K�
@K�F@K�@K"�@J^5@J-@JJ@I�@I��@I&�@I�@H��@HĜ@H��@HbN@H1'@H �@H �@H �@G��@G;d@F��@F�R@F�+@FV@F@E�T@E?}@D�@Dj@DI�@C�
@Ct�@B�!@B^5@B=q@BJ@A�#@A�^@A�7@AX@A7L@@�`@@A�@@1'@@ �@@b@@b@@b@@b@?\)@>�y@>�R@>@=��@=p�@=�@<��@<�@<�/@<�@<j@<9X@<1@;��@;�
@;ƨ@;�F@;��@;�@;t�@;C�@;@:�H@:�!@:~�@:-@9��@9�#@9��@9X@8��@8 �@7�;@7��@7�P@7l�@7+@6ȴ@6ff@65?@5�@5��@5O�@5�@4�/@4�j@4�D@4j@4j@4Z@49X@41@3ƨ@3�@3S�@333@3o@2�@2��@2^5@2=q@1�#@1�7@1x�@1�@0��@0��@0r�@0Q�@01'@0 �@0  @/�w@/�P@/K�@/K�@/+@.�y@.�y@.�R@.�R@.�+@.E�@.@-�-@-`B@,��@,�D@,(�@,1@+ƨ@+dZ@+o@*��@*�\@*=q@)��@)G�@(Ĝ@(r�@(1'@(b@'�w@&��@&��@&v�@&E�@&{@%@%�h@%`B@%?}@%V@$�/@$Z@$(�@#�
@#�F@#��@#��@#��@#��@#�@#t�@#33@#@"~�@"M�@!�#@!�^@!X@!%@ ��@ ��@ Ĝ@ Ĝ@ �9@ �@ r�@ bN@ A�@  �@   @   @�@|�@K�@
=@�y@�R@ff@5?@$�@�@��@�-@`B@O�@�@��@�/@�j@�D@j@I�@(�@��@ƨ@��@dZ@@�!@�\@~�@n�@^5@�@��@x�@hs@7L@%@��@�`@�`@�9@1'@�@��@�w@��@��@��@��@��@�P@\)@+@
=@ȴ@�+@E�@@�@�@��@p�@?}@�/@��@9X@��@C�@@��@^5@�@J@��@�#@�7@hs@��@�9@�9@��@�u@�u@�@r�@r�@A�@ �@��@��@l�@K�@;d@�@�@�R@E�@��@��@�@�@`B@�@�@�@��@�j@��@��@�D@�D@�D@�D@z�@I�@��@ƨ@��@t�@dZ@C�@o@
��@
�!@
�!@
�\@
n�@
=q@
J@	�@	��@	x�@	x�@	X@	G�@	7L@��@Ĝ@��@A�@�@�@�P@l�@+@��@�y@ȴ@�R@��@V@{@{@�@@p�@p�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB��B��B��B��B��B��B��B��B��B��B��B��B��B��B�?B�?B�XB�jB�jB�jB�qB�qB�dB�XB�LB�RB��B�TB�NB�BB1B	7B
=B�B'�B6FB>wB>wBJ�BK�BK�BZBgmBm�Br�Bz�Bw�Bt�By�Bs�Bo�Bm�Bn�Bn�Bo�Bo�Bn�Bl�BffBe`BdZBbNBaHB_;B^5B]/BS�BN�BJ�BM�BE�BA�B<jB49B+B�B\B1BB�B�B�HB�B��B�qB��B�7BbNBT�BD�B=qB1'B%�B�BB
�B
��B
�'B
��B
��B
��B
��B
�hB
�PB
�%B
� B
r�B
ffB
YB
D�B
<jB
49B
,B
%�B
�B
hB
B	��B	�NB	�B	��B	ȴB	��B	�^B	�3B	�B	��B	��B	�{B	�PB	�B	u�B	`BB	T�B	L�B	D�B	?}B	2-B	)�B	#�B	�B	�B	�B	�B	�B	�B	�B	{B	JB	1B	B	B	B	B	  B��B��B�B�B�B�B�sB�ZB�HB�/B�)B�B��B��BȴBB�wB�dB�9B�B�B�B�B��B��B��B��B��B��B��B��B�uB�hB�bB�\B�PB�DB�7B�+B�B�B�B~�B|�B{�Bx�Bu�Bs�Br�Bp�Bn�Bm�Bl�Bk�BjBiyBhsBgmBe`BbNB]/BZBZBYBYBXBXBW
BW
BVBT�BS�BR�BQ�BP�BO�BL�BK�BJ�BH�BD�BB�BA�B@�B>wB:^B6FB49B2-B1'B/B.B,B)�B(�B%�B%�B%�B$�B$�B"�B"�B!�B!�B!�B!�B!�B �B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B{B�BuB�B�B�B�B�B{B{B{BuBuB{B{B{B{B{B{B�B�B�B�B�B�B�B�B�B�B�B�B �B �B!�B!�B"�B"�B"�B#�B$�B&�B(�B,B.B/B/B0!B2-B49B49B6FB7LB7LB;dB=qB@�BA�BB�BB�BB�BD�BF�BG�BH�BL�BM�BM�BP�BR�BT�BXBYBYB[#B]/BdZBffBgmBgmBhsBhsBhsBhsBiyBk�Bo�Bq�Bs�Bs�Bt�Bu�Bx�Bz�B{�B|�B}�B� B�B�B�B�1B�VB�uB��B��B��B��B��B��B��B��B��B��B��B�B�B�!B�'B�9B�RB�jB�wB�wB�}B��BBÖBĜBƨB��B��B��B�B�)B�5B�5B�;B�NB�`B�mB�B�B�B�B�B��B��B��B��B��B	B	B	%B	+B		7B	
=B	PB	hB	uB	�B	�B	�B	�B	�B	�B	 �B	#�B	%�B	'�B	(�B	+B	-B	/B	1'B	33B	49B	7LB	7LB	7LB	:^B	=qB	?}B	?}B	?}B	?}B	B�B	D�B	D�B	F�B	H�B	I�B	J�B	J�B	K�B	L�B	M�B	N�B	N�B	O�B	Q�B	R�B	VB	XB	XB	YB	YB	[#B	]/B	^5B	_;B	`BB	`BB	aHB	cTB	gmB	iyB	k�B	n�B	q�B	s�B	t�B	u�B	x�B	z�B	{�B	{�B	{�B	{�B	|�B	|�B	}�B	}�B	~�B	� B	� B	�B	�B	�=B	�PB	�VB	�bB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�9B	�?B	�?B	�?B	�FB	�LB	�XB	�^B	�dB	�dB	�jB	�jB	�jB	�wB	�}B	��B	��B	��B	B	B	ÖB	ĜB	ƨB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�#B	�)B	�)B	�)B	�)B	�)B	�)B	�)B	�/B	�5B	�;B	�HB	�TB	�TB	�TB	�TB	�TB	�TB	�ZB	�mB	�mB	�mB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
%B
+B
+B
1B
1B
1B
1B
1B
1B
1B
	7B
	7B

=B

=B

=B
JB
JB
JB
JB
JB
PB
VB
VB
VB
\B
bB
bB
hB
hB
hB
oB
oB
oB
oB
oB
uB
uB
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
!�B
!�B
 �B
"�B
#�B
#�B
%�B
%�B
&�B
&�B
'�B
'�B
'�B
'�B
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
+B
+B
+B
,B
,B
-B
-B
-B
.B
.B
.B
0!B
0!B
1'B
1'B
1'B
1'B
2-B
2-B
33B
33B
49B
49B
49B
5?B
5?B
5?B
6FB
6FB
6FB
6FB
6FB
7LB
7LB
7LB
8RB
8RB
8RB
8RB
9XB
9XB
9XB
:^B
:^B
;dB
;dB
;dB
<jB
<jB
<jB
<jB
<jB
=qB
=qB
=qB
=qB
>wB
>wB
>wB
?}B
?}B
?}B
?}B
?}B
@�B
@�B
A�B
A�B
B�B
B�B
C�B
C�B
D�B
E�B
E�B
E�B
F�B
G�B
G�B
H�B
H�B
I�B
I�B
J�B
K�B
K�B
K�B
L�B
L�B
M�B
M�B
M�B
M�B
M�B
N�B
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
Q�B
Q�B
R�B
R�B
R�B
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
T�B
T�B
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
XB
XB
XB
XB
XB
YB
YB
YB
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
\)B
\)B
]/B
]/B
]/B
]/B
]/B
^5B
^5B
^5B
^5B
_;B
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
aHB
aHB
aHB
aHB
aHB
aHB
bNB
bNB
bNB
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
ffB
ffB
gmB
gmB
hsB
hsB
hsB
hsB
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
jB
k�B
k�B
k�B
l�B
l�B
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
n�B
o�B
o�B
o�B
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
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
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
u�B
v�B
v�B
v�B
v�B
v�B
w�B
w�B
w�B
w�B
w�B
x�B
x�B
x�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B�{B��B��B��B��B��B��B��B��B��B��B��B��B��B�'B�3B�3B�FB�FB�LB�LB�RB�FB�9B�?B�9B�}B�5B�)B�mB��BBBB{B$�B2-B9XB<jBD�BF�BF�BS�BaHBhsBm�Bu�Br�Bq�Bv�Bo�BjBiyBiyBjBjBjBk�BiyB_;B`BB_;B]/B\)BZBXBZBN�BK�BF�BH�B?}B<jB8RB0!B'�B{B
=BB��B�B�sB�/B�B��B�^B��B�1B\)BP�B?}B9XB,B"�B{BB
�B
�jB
�B
��B
��B
��B
�bB
�JB
�1B
�B
|�B
n�B
bNB
W
B
>wB
7LB
/B
&�B
!�B
�B
PB
B	�B	�)B	��B	��B	ÖB	�dB	�?B	�B	��B	��B	��B	�\B	�7B	}�B	t�B	[#B	P�B	G�B	?}B	=qB	-B	$�B	�B	�B	�B	�B	�B	�B	�B	�B	hB	+B	B��B��B��B��B��B��B�B�B�B�sB�ZB�TB�;B�)B�
B�
B��B��BǮBĜB�qB�XB�XB�B��B��B��B��B��B��B��B��B��B�{B�hB�bB�VB�DB�=B�=B�1B�+B�B�B~�B}�B{�Bx�Bv�Bw�Bt�Bo�Bm�Bm�Bk�BiyBhsBffBe`BdZBcTBbNBbNBaHB_;BXBR�BS�BR�BR�BQ�BQ�BP�BP�BO�BO�BN�BL�BK�BK�BJ�BF�BF�BE�BD�B@�B<jB;dB;dB;dB7LB2-B/B-B,B)�B(�B&�B$�B#�B!�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B{BuBoBhBhBbBhBbB\B\B\B\B\BbBbB\BVB\BVBVBVBVBVBVB\BbBbBbBbBbBhBoBuB{B{B�B�B�B�B�B�B�B�B�B�B�B�B!�B#�B%�B'�B(�B)�B+B,B.B/B1'B1'B2-B5?B8RB:^B;dB<jB<jB<jB>wB@�BA�BC�BF�BG�BH�BJ�BL�BN�BQ�BR�BR�BVBXB^5B`BBaHBaHBbNBbNBbNBbNBdZBffBiyBk�Bm�Bm�Bn�Bo�Br�Bt�Bu�Bv�Bw�By�B{�B|�B}�B�B�1B�PB�\B�uB��B��B��B��B��B��B��B��B��B��B��B��B�B�B�-B�FB�RB�RB�XB�^B�jB�qB�wB��BŢBɺB��B��B�B�B�B�B�)B�;B�HB�ZB�mB�B�B�B�B�B�B��B��B��B��B	  B	B	B	B	+B	DB	PB	bB	hB	{B	�B	�B	�B	�B	�B	�B	!�B	"�B	$�B	&�B	(�B	+B	-B	.B	1'B	1'B	1'B	49B	7LB	9XB	9XB	9XB	9XB	<jB	>wB	>wB	@�B	B�B	C�B	D�B	D�B	E�B	F�B	G�B	H�B	H�B	I�B	K�B	L�B	O�B	Q�B	Q�B	R�B	R�B	T�B	W
B	XB	YB	ZB	ZB	[#B	]/B	aHB	cTB	e`B	hsB	k�B	m�B	n�B	o�B	r�B	t�B	u�B	u�B	u�B	u�B	v�B	v�B	w�B	w�B	x�B	y�B	y�B	{�B	~�B	�B	�+B	�1B	�=B	�PB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�'B	�3B	�9B	�?B	�?B	�FB	�FB	�FB	�RB	�XB	�^B	�dB	�dB	�jB	�jB	�qB	�wB	��B	B	ĜB	ƨB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�
B	�B	�B	�#B	�/B	�/B	�/B	�/B	�/B	�/B	�5B	�HB	�HB	�HB	�NB	�TB	�TB	�ZB	�`B	�`B	�`B	�fB	�fB	�fB	�mB	�mB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
%B
%B
+B
1B
1B
1B
	7B

=B

=B
DB
DB
DB
JB
JB
JB
JB
JB
PB
PB
VB
VB
\B
\B
bB
bB
hB
oB
oB
oB
oB
oB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
 �B
!�B
!�B
!�B
!�B
"�B
"�B
"�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
$�B
$�B
$�B
%�B
%�B
&�B
&�B
&�B
'�B
'�B
'�B
)�B
)�B
+B
+B
+B
+B
,B
,B
-B
-B
.B
.B
.B
/B
/B
/B
0!B
0!B
0!B
0!B
0!B
1'B
1'B
1'B
2-B
2-B
2-B
2-B
33B
33B
33B
49B
49B
5?B
5?B
5?B
6FB
6FB
6FB
6FB
6FB
7LB
7LB
7LB
7LB
8RB
8RB
8RB
9XB
9XB
9XB
9XB
9XB
:^B
:^B
;dB
;dB
<jB
<jB
=qB
=qB
>wB
?}B
?}B
?}B
@�B
A�B
A�B
B�B
B�B
C�B
C�B
D�B
E�B
E�B
E�B
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
I�B
I�B
I�B
I�B
I�B
I�B
J�B
J�B
K�B
K�B
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
P�B
P�B
P�B
Q�B
Q�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
R�B
R�B
S�B
S�B
S�B
S�B
S�B
T�B
T�B
T�B
T�B
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
YB
YB
YB
YB
YB
ZB
ZB
ZB
[#B
[#B
[#B
[#B
[#B
[#B
[#B
[#B
[#B
[#B
\)B
\)B
\)B
]/B
]/B
]/B
]/B
]/B
^5B
^5B
_;B
_;B
`BB
`BB
`BB
aHB
aHB
bNB
bNB
bNB
bNB
cTB
cTB
cTB
dZB
dZB
dZB
dZB
dZB
dZB
dZB
dZB
dZB
e`B
e`B
e`B
ffB
ffB
ffB
ffB
ffB
ffB
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
iyB
iyB
iyB
iyB
iyB
iyB
iyB
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
n�B
n�B
n�B
n�B
o�B
o�B
o�B
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
r�B
r�B
r�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  =ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=m�h=m�h=m�h=m�h=m�h=m�h=m�h=m�h=m�h=m�h=m�h=m�h=m�h=m�h=m�h=m�h=m�h=m�h=m�h=m�h=m�h=m�h=m�h=m�h=m�h=m�h=m�h=m�h=m�h=m�h=m�h=m�h=m�h=m�h=m�h=m�h=m�h=m�h=m�h=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`BPRES            TEMP            PSAL            PRES_ADJUSTED = PRES - (REPORTED_SURFACE_PRESSURE) for PADJ                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            PSAL_ADJ corrects CTD thermal lag (CTL) viz. Johnson et al, 2007, JAOT, effects of pressure adjustments, and PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment term r.                  PADJ REPORTED_SURFACE_PRESSURE =0.08 dbar                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            CTL alpha = 0.021 & tau = 21 s with error equal to |correction| and for OW r = 0.9998 (+/-0.0014), vertically averaged dS = -0.006 (+/-0.055)                                                                                                                   Pressures adjusted using reported surface pressure when warranted.                                                                                                                                                                                              none                                                                                                                                                                                                                                                            After pressure and cell thermal lag correction of salinity values, OW correction estimated using mapping scales of 8 & 4 long. and 4 & 2 lat., no PV constraint, and decorrelation time scale of 10 years.                                                      202307212300282023072123002820230721230028  AO  ARCAADJP                                                                    20200628220059    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20200628220059  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20200628220059  QCF$                G�O�G�O�G�O�0               PM  ARSQPADJV1.1                                                                20230712105800  QC  PRES            @���D���G�O�                PM  ARSQCTM V1.1                                                                20230712105800  QC  PSAL            @���D���G�O�                PM  ARSQCOWGV1.1CTD_2021v2 + Argo_2021v03                                       20230721230927  IP                  G�O�G�O�G�O�                