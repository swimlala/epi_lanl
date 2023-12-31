CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2020-09-26T12:00:36Z creation      
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
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  `�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  pL   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �|   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �(   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �l   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �X   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �4   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �4   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �4   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �4   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �`   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �d   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �h   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �l   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �p   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��        ��Argo profile    3.1 1.2 19500101000000  20200926120036  20230721230928  4901659 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               �A   AO  5382                            2C  D   NAVIS_A                         0371                            082713                          863 @�;F��/1   @�;�d��@<j~��#�d-V1   GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                      �A   A   A   @�  @�  @���A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B'��B0  B8ffB@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'�fD(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]fD]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�ɚD���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�p�@�p�@�
>A�RA>�RA^�RA~�RA�\)A�\)A�\)A�\)A�\)A�\)A�\)A�\)B�B�B�B�B'G�B/�B8zB?�BG�BO�BW�B_�Bg�Bo�Bw�B�B��
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
B���B��
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
��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��D z�D ��D!z�D!��D"z�D"��D#z�D#��D$z�D$��D%z�D%��D&z�D&��D'�GD'��D(z�D(��D)z�D)��D*z�D*��D+z�D+��D,z�D,��D-z�D-��D.z�D.��D/z�D/��D0z�D0��D1z�D1��D2z�D2��D3z�D3��D4z�D4��D5z�D5��D6z�D6��D7z�D7��D8z�D8��D9z�D9��D:z�D:��D;z�D;��D<z�D<��D=z�D=��D>z�D>��D?z�D?��D@z�D@��DAz�DA��DBz�DB��DCz�DC��DDz�DD��DEz�DE��DFz�DF��DGz�DG��DHz�DH��DIz�DI��DJz�DJ��DKz�DK��DLz�DL��DMz�DM��DNz�DN��DOz�DO��DPz�DP��DQz�DQ��DRz�DR��DSz�DS��DTz�DT��DUz�DU��DVz�DV��DWz�DW��DXz�DX��DYz�DY��DZz�DZ��D[z�D[��D\z�D]GD]z�D]��D^z�D^��D_z�D_��D`z�D`��Daz�Da��Dbz�Db��Dcz�Dc��Ddz�Dd��Dez�De��Dfz�Df��Dgz�Dg��Dhz�Dh��Diz�Di��Djz�Dj��Dkz�Dk��Dlz�Dl��Dmz�Dm��Dnz�Dn��Doz�Do��Dpz�Dp��Dqz�Dq��Drz�Dr��Dsz�Ds��Dtz�Dt��Duz�Du��Dvz�Dv��Dwz�Dw��Dxz�Dx��Dyz�Dy��Dzz�Dz��D{z�D{��D|z�D|��D}z�D}��D~z�D~��Dz�D��D�=qD�}qD��qD��qD�:>D�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�@�D�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD� �D�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD½qD��qD�=qD�}qDýqD��qD�=qD�}qDĽqD��qD�=qD�}qDŽqD��qD�=qD�}qDƽqD��qD�=qD�}qDǽqD��qD�=qD�}qDȽqD��qD�=qD�}qDɽqD��qD�=qD�}qDʽqD��qD�=qD�}qD˽qD��qD�=qD�}qD̽qD��qD�=qD�}qDͽqD��qD�=qD�}qDνqD��qD�=qD�}qDϽqD��qD�=qD�}qDнqD��qD�=qD�}qDѽqD��qD�=qD�}qDҽqD��qD�=qD�}qDӽqD��qD�=qD�}qDԽqD��qD�=qD�}qDսqD��qD�=qD�}qDֽqD��qD�=qD�}qD׽qD��qD�=qD�}qDؽqD��qD�=qD�}qDٽqD��qD�=qD�}qDڽqD��qD�=qD�}qD۽qD��qD�=qD�}qDܽqD��qD�=qD�}qDݽqD��qD�=qD�}qD޽qD��qD�=qD�}qD߽qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD��qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��D��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�ȴA�ƨA�ĜA���A���A���A���A���A١�Aٝ�Aٗ�Aُ\AًDA�x�A�=qA�7LAׇ+A�AԼjA�I�A��;A�bAǣ�Aĉ7AÇ+A�JA���A���A�ȴA�`BA���A�S�A��A��`A��TA��DA��yA��
A��A��A��FA�{A�(�A���A�/A��A��DA�1'A��HA���A��uA�O�A�VA��jA���A�VA�{A���A��A���A�?}A�9XA���A��A��A���A��7A�Q�A�n�A�ƨA���A���A�XA�p�A���A���A�dZA���A��^A�+A��A���A��A�A��A�(�A�l�A�33A�hsAVA~  A|�\A{x�Ax�yAw�Au`BAqS�AnĜAl��Aj�RAiK�Af��Ad�`Ac�A`�RA_�^A^ffA]O�A\�yA[�AZȴAZ��AY��AV�yAT�AS��AS�PASl�AR�yAQAQ&�AP=qAOVAN�ALn�AKVAJĜAIAHM�AF��AEl�AEO�AE;dAC|�ABr�AA�#AA��AA�7AAdZA?��A=��A=\)A<z�A<{A;��A;�A9�
A9A8r�A81A7`BA6�A69XA5��A5��A4��A2�`A21'A1�-A1+A0�uA01A.�A-��A,ȴA,VA,  A+�-A+O�A*�yA*E�A)x�A(^5A'��A'��A'�A'dZA&�A%�mA%?}A$ĜA$~�A#�^A"�!A"$�A!�FA!�hA!C�A �A (�A�
A�7A
=A��A~�AbNA�A�A�;A�FA��At�A?}A�Av�A{A�TA�^AK�A��A�;A�A�jA$�A�7A(�A��A-A�HAt�A%A�A|�AffA�AhsA
�/A
r�A	��A	��A�jA�A(�A��A��A;dA�A V@�ƨ@���@�5?@��m@�t�@��-@�S�@�E�@�p�@���@�Ĝ@��
@��@��^@�%@���@���@�^@��/@�j@�ȴ@���@���@� �@���@ް!@�9X@�t�@�
=@�V@ش9@׮@��@�1'@��y@���@�^5@�p�@Ь@�1@θR@̃@ʸR@�5?@��@��@ə�@�V@ȴ9@��;@ř�@þw@�^5@��-@��w@��@��h@��/@�z�@�1@�ȴ@�=q@�-@�{@��@���@��-@�hs@���@���@���@�^5@�`B@���@�Z@��+@�b@��/@��@��H@�{@���@�1@��\@���@�%@�1@���@�E�@��@���@���@���@���@���@�9X@���@�C�@�+@�
=@��!@���@�v�@�V@���@�X@���@��@��@���@��y@�M�@��#@�hs@���@�z�@�I�@�  @�\)@�o@�ȴ@���@�~�@�^5@�-@���@�@��h@�7L@��`@��9@�Q�@��@��
@��w@��@�t�@�S�@�
=@���@�M�@��^@�7L@�%@��9@��D@�r�@��
@��P@�l�@�\)@�;d@��@���@�ff@�E�@�E�@�E�@�$�@�J@���@�O�@���@��D@�A�@�1@���@�;d@��y@���@�=q@�@��@��@�r�@� �@�1@��;@��@�\)@�
=@�@��@��@��@�ȴ@���@��\@��+@�v�@��@��h@�hs@���@���@�Ĝ@��D@�Q�@�b@��@��
@��F@���@�t�@�;d@�"�@���@��R@�ff@���@���@���@��@��@���@��D@�r�@�Z@�1'@\)@~��@~5?@}��@}`B@}?}@}V@|�@|��@|�@{�F@{�@{C�@z�@z�!@z^5@zJ@y��@y7L@x��@x�u@xA�@xb@w�@w��@w+@v��@vv�@vV@vE�@v{@u�T@u/@t9X@s��@s�
@sƨ@s�F@s�F@s��@s�@sdZ@s33@s"�@r�@r��@r��@r�!@r��@r~�@r=q@q�#@q&�@pbN@o�@o�@nȴ@n�R@n��@n��@n��@nv�@nff@n$�@mp�@lI�@k��@k"�@j�\@i%@h��@hbN@hb@h  @h  @g��@g��@g�P@g|�@gl�@g\)@gK�@g+@f��@fE�@e�h@d��@d��@dj@d9X@c��@c�
@c��@cS�@b�@a�@`�`@`Q�@`A�@`A�@_��@_;d@_+@^��@^�R@^@]/@\I�@\�@[�m@[�F@[t�@[33@[o@[@[@[@[@Z�!@Y�@Y&�@Y�@X��@X��@XĜ@X�9@X�9@X�9@X�u@X�u@Xr�@X1'@W�@V�y@Vȴ@V��@V��@V�+@Vv�@V5?@U@U��@Up�@U�@T�@T��@TI�@T9X@T(�@S�m@S��@SdZ@R�H@R�!@R��@Rn�@RJ@Qx�@QG�@Q&�@P��@P�`@P�`@P��@PĜ@P�9@P��@P�u@P�u@P��@P��@P�@PbN@PQ�@P  @O�P@O+@N��@N�+@M�@M�T@M�T@M�T@M@M`B@L�/@L�j@Lj@LI�@L1@J�@J-@I��@Ix�@Ix�@Ihs@I&�@Hr�@G��@G;d@Fȴ@Fff@E�@E`B@EV@D�/@D�@D9X@C��@CS�@C"�@C@B�H@B��@B�\@B~�@Bn�@B�@A�7@A7L@@��@@��@@��@@��@@�9@@�u@@r�@@1'@@  @?�;@?��@?;d@?�@>�R@=��@=/@<�D@;��@;S�@;"�@:�@:�H@:��@:��@:^5@:�@9��@9hs@8��@8Q�@8 �@7�@7��@7�w@7��@7��@7|�@7|�@7�@6�+@6V@6$�@6@5��@5�@5`B@5�@4�@4�D@4Z@3ƨ@3C�@2~�@1��@17L@0��@0r�@0bN@0A�@/�@/��@/l�@/+@/
=@.�@.ff@.$�@.@-�@-�T@-@-��@-`B@-/@,�/@,1@+�
@+��@+dZ@+dZ@+"�@*��@*�\@*^5@*J@)7L@)%@(��@(��@(r�@(Q�@(1'@(  @'��@'l�@'K�@';d@'+@'
=@&�y@&�R@&��@&v�@&$�@%�h@%V@$�@$�/@$��@$�j@$�@$�D@$Z@$�@#�
@#t�@#C�@#o@#"�@#o@#@"�H@"~�@"J@!�@!�@!��@!��@!7L@ ��@ 1'@ b@   @��@�P@;d@��@�@ȴ@��@�+@ff@ff@ff@V@V@E�@E�@5?@5?@$�@5?@5?@5?@{@�@�T@�T@@�@`B@O�@?}@?}@�@��@�@�@�@��@�j@�j@��@�D@�D@j@(�@1@��@�
@��@dZ@C�@@�!@~�@^5@M�@J@�#@��@hs@��@b@��@��@�P@l�@\)@\)@+@�@ȴ@ȴ@ȴ@��@V@ff@ff@ff@ff@ff@ff@ff@E�@5?@�-@�/@(�@��@S�@33@o@@�H@�!@M�@��@hs@&�@�`@��@Ĝ@��@�u@r�@bN@bN@bN@Q�@A�@A�@1'@1'@1'@ �@b@b@b@  @�@�;@�w@�w@��@�w@�@�@��@��@|�@K�@�@��@v�@V@{@��@@�h@O�@?}@�@��@�j@�D@z�@Z@��@�m@��@t�@S�@C�@33@o@o@@
�@
�@
�H@
��@
��@
^5@
J@	�#@	�7@	�@Ĝ@��@bN@ �@�w@l�@;d@�@��@�@�R@�R@ȴ@�R@{@@�T@@�-@�h@O�@��@��@��@j@Z@Z@Z1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�ȴA�ƨA�ĜA���A���A���A���A���A١�Aٝ�Aٗ�Aُ\AًDA�x�A�=qA�7LAׇ+A�AԼjA�I�A��;A�bAǣ�Aĉ7AÇ+A�JA���A���A�ȴA�`BA���A�S�A��A��`A��TA��DA��yA��
A��A��A��FA�{A�(�A���A�/A��A��DA�1'A��HA���A��uA�O�A�VA��jA���A�VA�{A���A��A���A�?}A�9XA���A��A��A���A��7A�Q�A�n�A�ƨA���A���A�XA�p�A���A���A�dZA���A��^A�+A��A���A��A�A��A�(�A�l�A�33A�hsAVA~  A|�\A{x�Ax�yAw�Au`BAqS�AnĜAl��Aj�RAiK�Af��Ad�`Ac�A`�RA_�^A^ffA]O�A\�yA[�AZȴAZ��AY��AV�yAT�AS��AS�PASl�AR�yAQAQ&�AP=qAOVAN�ALn�AKVAJĜAIAHM�AF��AEl�AEO�AE;dAC|�ABr�AA�#AA��AA�7AAdZA?��A=��A=\)A<z�A<{A;��A;�A9�
A9A8r�A81A7`BA6�A69XA5��A5��A4��A2�`A21'A1�-A1+A0�uA01A.�A-��A,ȴA,VA,  A+�-A+O�A*�yA*E�A)x�A(^5A'��A'��A'�A'dZA&�A%�mA%?}A$ĜA$~�A#�^A"�!A"$�A!�FA!�hA!C�A �A (�A�
A�7A
=A��A~�AbNA�A�A�;A�FA��At�A?}A�Av�A{A�TA�^AK�A��A�;A�A�jA$�A�7A(�A��A-A�HAt�A%A�A|�AffA�AhsA
�/A
r�A	��A	��A�jA�A(�A��A��A;dA�A V@�ƨ@���@�5?@��m@�t�@��-@�S�@�E�@�p�@���@�Ĝ@��
@��@��^@�%@���@���@�^@��/@�j@�ȴ@���@���@� �@���@ް!@�9X@�t�@�
=@�V@ش9@׮@��@�1'@��y@���@�^5@�p�@Ь@�1@θR@̃@ʸR@�5?@��@��@ə�@�V@ȴ9@��;@ř�@þw@�^5@��-@��w@��@��h@��/@�z�@�1@�ȴ@�=q@�-@�{@��@���@��-@�hs@���@���@���@�^5@�`B@���@�Z@��+@�b@��/@��@��H@�{@���@�1@��\@���@�%@�1@���@�E�@��@���@���@���@���@���@�9X@���@�C�@�+@�
=@��!@���@�v�@�V@���@�X@���@��@��@���@��y@�M�@��#@�hs@���@�z�@�I�@�  @�\)@�o@�ȴ@���@�~�@�^5@�-@���@�@��h@�7L@��`@��9@�Q�@��@��
@��w@��@�t�@�S�@�
=@���@�M�@��^@�7L@�%@��9@��D@�r�@��
@��P@�l�@�\)@�;d@��@���@�ff@�E�@�E�@�E�@�$�@�J@���@�O�@���@��D@�A�@�1@���@�;d@��y@���@�=q@�@��@��@�r�@� �@�1@��;@��@�\)@�
=@�@��@��@��@�ȴ@���@��\@��+@�v�@��@��h@�hs@���@���@�Ĝ@��D@�Q�@�b@��@��
@��F@���@�t�@�;d@�"�@���@��R@�ff@���@���@���@��@��@���@��D@�r�@�Z@�1'@\)@~��@~5?@}��@}`B@}?}@}V@|�@|��@|�@{�F@{�@{C�@z�@z�!@z^5@zJ@y��@y7L@x��@x�u@xA�@xb@w�@w��@w+@v��@vv�@vV@vE�@v{@u�T@u/@t9X@s��@s�
@sƨ@s�F@s�F@s��@s�@sdZ@s33@s"�@r�@r��@r��@r�!@r��@r~�@r=q@q�#@q&�@pbN@o�@o�@nȴ@n�R@n��@n��@n��@nv�@nff@n$�@mp�@lI�@k��@k"�@j�\@i%@h��@hbN@hb@h  @h  @g��@g��@g�P@g|�@gl�@g\)@gK�@g+@f��@fE�@e�h@d��@d��@dj@d9X@c��@c�
@c��@cS�@b�@a�@`�`@`Q�@`A�@`A�@_��@_;d@_+@^��@^�R@^@]/@\I�@\�@[�m@[�F@[t�@[33@[o@[@[@[@[@Z�!@Y�@Y&�@Y�@X��@X��@XĜ@X�9@X�9@X�9@X�u@X�u@Xr�@X1'@W�@V�y@Vȴ@V��@V��@V�+@Vv�@V5?@U@U��@Up�@U�@T�@T��@TI�@T9X@T(�@S�m@S��@SdZ@R�H@R�!@R��@Rn�@RJ@Qx�@QG�@Q&�@P��@P�`@P�`@P��@PĜ@P�9@P��@P�u@P�u@P��@P��@P�@PbN@PQ�@P  @O�P@O+@N��@N�+@M�@M�T@M�T@M�T@M@M`B@L�/@L�j@Lj@LI�@L1@J�@J-@I��@Ix�@Ix�@Ihs@I&�@Hr�@G��@G;d@Fȴ@Fff@E�@E`B@EV@D�/@D�@D9X@C��@CS�@C"�@C@B�H@B��@B�\@B~�@Bn�@B�@A�7@A7L@@��@@��@@��@@��@@�9@@�u@@r�@@1'@@  @?�;@?��@?;d@?�@>�R@=��@=/@<�D@;��@;S�@;"�@:�@:�H@:��@:��@:^5@:�@9��@9hs@8��@8Q�@8 �@7�@7��@7�w@7��@7��@7|�@7|�@7�@6�+@6V@6$�@6@5��@5�@5`B@5�@4�@4�D@4Z@3ƨ@3C�@2~�@1��@17L@0��@0r�@0bN@0A�@/�@/��@/l�@/+@/
=@.�@.ff@.$�@.@-�@-�T@-@-��@-`B@-/@,�/@,1@+�
@+��@+dZ@+dZ@+"�@*��@*�\@*^5@*J@)7L@)%@(��@(��@(r�@(Q�@(1'@(  @'��@'l�@'K�@';d@'+@'
=@&�y@&�R@&��@&v�@&$�@%�h@%V@$�@$�/@$��@$�j@$�@$�D@$Z@$�@#�
@#t�@#C�@#o@#"�@#o@#@"�H@"~�@"J@!�@!�@!��@!��@!7L@ ��@ 1'@ b@   @��@�P@;d@��@�@ȴ@��@�+@ff@ff@ff@V@V@E�@E�@5?@5?@$�@5?@5?@5?@{@�@�T@�T@@�@`B@O�@?}@?}@�@��@�@�@�@��@�j@�j@��@�D@�D@j@(�@1@��@�
@��@dZ@C�@@�!@~�@^5@M�@J@�#@��@hs@��@b@��@��@�P@l�@\)@\)@+@�@ȴ@ȴ@ȴ@��@V@ff@ff@ff@ff@ff@ff@ff@E�@5?@�-@�/@(�@��@S�@33@o@@�H@�!@M�@��@hs@&�@�`@��@Ĝ@��@�u@r�@bN@bN@bN@Q�@A�@A�@1'@1'@1'@ �@b@b@b@  @�@�;@�w@�w@��@�w@�@�@��@��@|�@K�@�@��@v�@V@{@��@@�h@O�@?}@�@��@�j@�D@z�@Z@��@�m@��@t�@S�@C�@33@o@o@@
�@
�@
�H@
��@
��@
^5@
J@	�#@	�7@	�@Ĝ@��@bN@ �@�w@l�@;d@�@��@�@�R@�R@ȴ@�R@{@@�T@@�-@�h@O�@��@��@��@j@Z@Z@Z1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB5?B6FB6FB6FB6FB6FB6FB6FB6FB6FB6FB5?B5?B49B0!B!�B�B%B  B��B��B�/B�RB��B��B�oBr�BdZB^5B`BB_;BZBN�BK�BJ�BJ�BH�BJ�BK�BK�BI�BC�B@�B?}BB�BI�BI�BG�BC�BA�B?}B<jB8RB2-B0!B+B%�B�BbB	7B��B�B�TB�
B��B��B�XB�B��B��B�7Bx�BcTB?}B)�B#�B�B%B
�B
�mB
��B
�}B
�9B
��B
��B
�=B
q�B
_;B
R�B
F�B
>wB
33B
(�B
�B
DB	��B	�B	�)B	��B	B	�RB	��B	��B	�PB	�B	{�B	u�B	q�B	n�B	iyB	e`B	cTB	_;B	S�B	I�B	E�B	D�B	B�B	@�B	:^B	6FB	-B	"�B	�B	�B	\B	PB	
=B	B��B��B��B		7B	B��B��B��B��B��B�B�B�sB�ZB�HB�;B�/B�
B��B��B��B��B��B��B��BȴB��B�^B�LB�LB�3B�B�B��B��B��B��B��B��B��B��B��B�hB�VB�PB�JB�JB�=B�7B�1B�%B�B�B}�Bz�Bx�Bw�Bw�Bv�Bt�Bs�Br�Bq�Bq�Bq�Bq�Bp�Bo�Bn�Bn�Bn�Bn�Bn�Bn�Bm�Bl�Bk�Bk�BjBhsBffBcTB_;B]/B[#BXBT�BP�BN�BK�BI�BH�BG�BF�BE�BD�BC�BB�BA�B@�B>wB<jB9XB7LB49B2-B/B,B)�B'�B$�B!�B�B�B�B�B�B�B�B�B�B�B�B�B�B{BuBoBhBhBbB\BVBJB
=BJBDB
=B1B1B+B%B+B1B1B1B	7B
=B
=BJB
=BJBVB\BuB�B�B�B�B!�B!�B�B �B$�B'�B)�B/B2-B2-B5?B6FB6FB6FB6FB6FB6FB6FB6FB6FB7LB:^B<jB<jB<jB=qB>wBE�BJ�BM�BP�BT�BW
B[#B^5B_;BcTBgmBjBk�Bl�Bl�Bn�Bq�Bq�Br�Bu�Bw�Bx�By�Bz�Bz�B{�B{�B|�B}�B� B�B�B�B�1B�DB�PB�VB�bB�uB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�-B�9B�RB�jB�qB�}B��B��BŢBǮBȴBȴBɺB��B��B��B��B��B��B��B��B��B�
B�B�#B�/B�;B�BB�ZB�`B�mB�B�B�B�B��B��B��B��B��B	  B	B	B	%B	+B	1B		7B	DB	JB	JB	JB	hB	{B	�B	�B	�B	�B	�B	 �B	#�B	$�B	%�B	&�B	'�B	)�B	,B	-B	/B	2-B	5?B	9XB	:^B	;dB	>wB	?}B	?}B	B�B	C�B	D�B	F�B	L�B	Q�B	VB	YB	\)B	\)B	\)B	^5B	^5B	`BB	bNB	cTB	dZB	ffB	gmB	iyB	k�B	m�B	o�B	p�B	r�B	s�B	t�B	t�B	v�B	x�B	z�B	{�B	|�B	|�B	}�B	~�B	�B	�+B	�1B	�7B	�7B	�7B	�=B	�=B	�=B	�DB	�JB	�JB	�PB	�VB	�VB	�VB	�\B	�\B	�bB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�'B	�RB	�XB	�^B	�dB	�jB	�jB	�jB	�qB	�wB	�wB	�}B	�}B	�}B	�}B	��B	ÖB	ŢB	ǮB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�
B	�B	�B	�B	�/B	�BB	�BB	�HB	�NB	�TB	�ZB	�ZB	�`B	�`B	�`B	�`B	�`B	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
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
B
B
B
B
B
B
B
B
B
%B
%B
1B
1B
1B
1B
1B
	7B
DB
DB
JB
JB
JB
PB
\B
bB
bB
bB
bB
bB
oB
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
#�B
$�B
%�B
&�B
'�B
'�B
'�B
'�B
'�B
'�B
(�B
(�B
)�B
)�B
+B
,B
-B
-B
.B
.B
.B
.B
.B
.B
/B
0!B
0!B
1'B
1'B
1'B
2-B
2-B
2-B
33B
33B
33B
49B
49B
5?B
7LB
7LB
8RB
9XB
9XB
9XB
9XB
9XB
:^B
;dB
;dB
;dB
<jB
<jB
=qB
=qB
=qB
=qB
=qB
>wB
>wB
>wB
@�B
@�B
@�B
A�B
A�B
A�B
B�B
B�B
B�B
B�B
D�B
D�B
D�B
E�B
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
G�B
H�B
H�B
H�B
H�B
I�B
J�B
J�B
J�B
J�B
K�B
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
M�B
M�B
N�B
O�B
O�B
O�B
O�B
O�B
O�B
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
VB
W
B
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
\)B
\)B
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
`BB
`BB
`BB
`BB
`BB
`BB
`BB
`BB
`BB
`BB
`BB
`BB
aHB
bNB
cTB
dZB
dZB
dZB
e`B
e`B
e`B
e`B
e`B
ffB
gmB
gmB
hsB
hsB
hsB
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
jB
jB
k�B
k�B
k�B
k�B
l�B
l�B
l�B
m�B
m�B
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
p�B
p�B
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
r�B
r�B
r�B
s�B
s�B
t�B
t�B
t�B
u�B
u�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
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
z�B
z�B
z�B
z�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B.B/B/B/B/B/B/B/B/B/B/B.B/B.B,B�B{BB��B��B��B�;B�?B��B��B��Br�Be`B[#B]/B\)B[#BM�BG�BD�BE�BD�BE�BF�BF�BD�B>wB:^B9XB<jBB�BC�BA�B<jB;dB9XB6FB2-B+B)�B$�B�B�B
=BB��B�`B�5B��BǮB�jB�FB��B��B�uB�Bu�BaHB;dB#�B�B�BB
�B
�TB
��B
�XB
�B
��B
��B
�1B
l�B
ZB
M�B
@�B
9XB
.B
$�B
hB
1B	��B	�fB	�B	ɺB	�qB	�?B	��B	�oB	�7B	{�B	v�B	p�B	k�B	iyB	cTB	_;B	^5B	]/B	N�B	C�B	?}B	>wB	<jB	;dB	49B	1'B	'�B	�B	�B	bB		7B	1B	B��B��B�B��B	B��B��B��B��B��B�B�B�TB�NB�5B�#B�B�B��B��B��B��B��B��BɺBŢBÖB�jB�9B�'B�'B�B��B��B��B��B��B��B��B��B�uB�oB�bB�JB�1B�+B�%B�%B�B�B�B� B}�B|�Bx�Bt�Br�Bq�Bq�Bp�Bn�Bm�Bl�Bk�Bk�Bk�Bk�Bk�BiyBgmBhsBhsBhsBhsBhsBgmBffBe`Be`BdZBbNB`BB]/BYBW
BVBR�BO�BJ�BI�BF�BC�BB�BB�BA�B?}B>wB=qB<jB;dB:^B9XB7LB49B2-B.B-B+B&�B#�B#�B �B�B�B�B�B{BuBuBoBoBhBbBbB\B\BVBPBJBJBDB
=B	7B
=B1BB%BBBBBBBBBBBBBB+BBB1B	7BPBoB{B�B�B�B�B�B�B�B!�B#�B(�B,B,B/B0!B0!B0!B0!B0!B0!B0!B0!B0!B1'B49B6FB6FB7LB8RB:^B?}BD�BG�BJ�BN�BQ�BT�BXBYB]/BaHBdZBe`BffBffBhsBk�Bk�Bl�Bo�Bq�Br�Bs�Bs�Bt�Bu�Bu�Bv�Bw�By�B{�B}�B~�B�B�B�+B�1B�=B�PB�VB�\B�oB�uB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�-B�FB�LB�XB�^B�dB�}B��BBBÖBŢBǮBȴBȴBɺBɺB��B��B��B��B��B��B�
B�B�B�5B�;B�HB�ZB�mB�yB�B�B�B��B��B��B��B��B��B	  B	  B	B	B	B	%B	%B	%B	DB	VB	\B	uB	{B	�B	�B	�B	�B	�B	�B	 �B	!�B	#�B	%�B	&�B	(�B	,B	/B	33B	49B	5?B	7LB	9XB	9XB	<jB	=qB	>wB	@�B	F�B	K�B	O�B	R�B	VB	VB	VB	W
B	XB	ZB	\)B	]/B	^5B	`BB	aHB	cTB	e`B	gmB	iyB	jB	l�B	m�B	n�B	n�B	p�B	r�B	t�B	t�B	u�B	v�B	w�B	x�B	{�B	� B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�%B	�%B	�+B	�1B	�+B	�7B	�7B	�=B	�JB	�\B	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�'B	�3B	�9B	�9B	�?B	�FB	�?B	�FB	�LB	�LB	�RB	�RB	�XB	�XB	�dB	�qB	�}B	��B	B	ÖB	ĜB	ĜB	ŢB	ŢB	ƨB	ƨB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�#B	�)B	�/B	�/B	�/B	�5B	�5B	�5B	�;B	�;B	�NB	�TB	�TB	�ZB	�ZB	�ZB	�ZB	�ZB	�`B	�`B	�fB	�fB	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
%B
%B
+B
	7B
	7B
	7B
	7B

=B

=B
JB
VB
VB
\B
bB
hB
hB
hB
oB
uB
uB
uB
{B
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
"�B
"�B
#�B
#�B
$�B
$�B
%�B
%�B
&�B
&�B
'�B
&�B
&�B
'�B
(�B
(�B
)�B
)�B
+B
)�B
+B
,B
,B
,B
-B
-B
.B
.B
/B
1'B
1'B
1'B
2-B
33B
33B
2-B
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
7LB
8RB
8RB
8RB
9XB
:^B
9XB
:^B
;dB
;dB
;dB
<jB
<jB
<jB
=qB
=qB
=qB
>wB
>wB
>wB
?}B
@�B
?}B
@�B
@�B
@�B
@�B
@�B
@�B
A�B
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
E�B
E�B
F�B
F�B
F�B
F�B
F�B
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
J�B
J�B
J�B
L�B
L�B
L�B
M�B
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
M�B
M�B
M�B
M�B
M�B
M�B
M�B
N�B
N�B
O�B
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
Q�B
Q�B
P�B
P�B
Q�B
R�B
Q�B
Q�B
R�B
S�B
R�B
R�B
S�B
T�B
S�B
S�B
T�B
VB
VB
XB
W
B
W
B
XB
XB
XB
YB
YB
YB
YB
YB
YB
ZB
YB
YB
YB
YB
YB
YB
YB
YB
YB
ZB
[#B
\)B
]/B
]/B
]/B
]/B
^5B
^5B
_;B
_;B
_;B
_;B
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
bNB
bNB
bNB
bNB
bNB
bNB
bNB
cTB
cTB
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
gmB
ffB
gmB
hsB
gmB
hsB
hsB
hsB
iyB
hsB
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
l�B
l�B
m�B
l�B
n�B
n�B
n�B
o�B
n�B
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
p�B
q�B
q�B
r�B
r�B
r�B
r�B
r�B
s�B
s�B
s�B
s�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 =m�h=m�h=m�h=m�h=m�h=m�h=m�h=m�h=m�h=m�h=m�h=m�h=m�h=m�h=m�h=m�h=m�h=m�h=m�h=m�h=m�h=m�h=m�h=m�h=m�h=m�h=m�h=m�h=m�h=m�h=m�h=m�h=m�h=m�h=m�h=m�h=m�h=m�h=m�h=m�h=m�h=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`BPRES            TEMP            PSAL            PRES_ADJUSTED = PRES - (REPORTED_SURFACE_PRESSURE) for PADJ                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            PSAL_ADJ corrects CTD thermal lag (CTL) viz. Johnson et al, 2007, JAOT, effects of pressure adjustments, and PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment term r.                  PADJ REPORTED_SURFACE_PRESSURE =0.08 dbar                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            CTL alpha = 0.021 & tau = 21 s with error equal to |correction| and for OW r = 0.9998 (+/-0.0014), vertically averaged dS = -0.007 (+/-0.055)                                                                                                                   Pressures adjusted using reported surface pressure when warranted.                                                                                                                                                                                              none                                                                                                                                                                                                                                                            After pressure and cell thermal lag correction of salinity values, OW correction estimated using mapping scales of 8 & 4 long. and 4 & 2 lat., no PV constraint, and decorrelation time scale of 10 years.                                                      202307212300282023072123002820230721230028  AO  ARCAADJP                                                                    20200926120036    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20200926120036  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20200926120036  QCF$                G�O�G�O�G�O�0               PM  ARSQPADJV1.1                                                                20230712105805  QC  PRES            @�  D���G�O�                PM  ARSQCTM V1.1                                                                20230712105805  QC  PSAL            @�  D���G�O�                PM  ARSQCOWGV1.1CTD_2021v2 + Argo_2021v03                                       20230721230928  IP                  G�O�G�O�G�O�                