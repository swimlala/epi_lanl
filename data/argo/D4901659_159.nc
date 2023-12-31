CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-12-06T10:00:29Z creation      
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
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M    PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  `�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  p4   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �h   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �T   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �,   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ݼ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �    HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �$   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �(   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �h   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �x   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �|   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �        �Argo profile    3.1 1.2 19500101000000  20181206100029  20230721230919  4901659 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               �A   AO  5382                            2C  D   NAVIS_A                         0371                            082713                          863 @ؖ�t�z1   @ؖs��L@;�?|�h�cs�vȴ91   GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                      �A   A   A   @�33@�  A   A   A@  A`  A�  A�  A���A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�y�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @���@�p�@�p�A�RA>�RA^�RA~�RA�\)A�(�A�\)A�\)A�\)A�\)A�\)A�\)B�B�B�B�B'�B/�B7�B?�BG�BO�BW�B_�Bg�Bo�Bw�B�B��
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
C�C�C�C�C	�C�C�C�C�C�C�C�C�C�C�C�C!�C#�C%�C'�C)�C+�C-�C/�C1�C3�C5�C7�C9�C;�C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D z�D ��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��D	z�D	��D
z�D
��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��D z�D ��D!z�D!��D"z�D"��D#z�D#��D$z�D$��D%z�D%��D&z�D&��D'z�D'��D(z�D(��D)z�D)��D*z�D*��D+z�D+��D,z�D,��D-z�D-��D.z�D.��D/z�D/��D0z�D0��D1z�D1��D2z�D2��D3z�D3��D4z�D4��D5z�D5��D6z�D6��D7z�D7��D8z�D8��D9z�D9��D:z�D:��D;z�D;��D<z�D<��D=z�D=��D>z�D>��D?z�D?��D@z�D@��DAz�DA��DBz�DB��DCz�DC��DDz�DD��DEz�DE��DFz�DF��DGz�DG��DHz�DH��DIz�DI��DJz�DJ��DKz�DK��DLz�DL��DMz�DM��DNz�DN��DOz�DO��DPz�DP��DQz�DQ��DRz�DR��DSz�DS��DTz�DT��DUz�DU��DVz�DV��DWz�DW��DXz�DX��DYz�DY��DZz�DZ��D[z�D[��D\z�D\��D]z�D]��D^z�D^��D_z�D_��D`z�D`��Daz�Da��Dbz�Db��Dcz�Dc��Ddz�Dd��Dez�De��Dfz�Df��Dgz�Dg��Dhz�Dh��Diz�Di��Djz�Dj��Dkz�Dk��Dlz�Dl��Dmz�Dm��Dnz�Dn��Doz�Do��Dpz�Dp��Dqz�Dq��Drz�Dr��Dsz�Ds��Dtz�Dt��Duz�Du��Dvz�Dv��Dwz�Dw��Dxz�Dx��Dyz�Dy��Dzz�Dz��D{z�D{��D|z�D|��D}z�D}��D~z�D~��Dz�D��D�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�@�D�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��>D�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD½qD��qD�=qD�}qDýqD��qD�=qD�}qDĽqD��qD�=qD�}qDŽqD��qD�=qD�}qDƽqD��qD�=qD�}qDǽqD��qD�=qD�}qDȽqD��qD�=qD�}qDɽqD��qD�=qD�}qDʽqD��qD�=qD�}qD˽qD��qD�=qD�}qD̽qD��qD�=qD�}qDͽqD��qD�=qD�}qDνqD��qD�=qD�}qDϽqD��qD�=qD�}qDнqD��qD�=qD�}qDѽqD��qD�=qD�}qDҽqD��qD�=qD�}qDӽqD��qD�=qD�}qDԽqD��qD�=qD�}qDսqD��qD�=qD�}qDֽqD��qD�=qD�}qD׽qD��qD�=qD�}qDؽqD��qD�=qD�}qDٽqD��qD�=qD�}qDڽqD��qD�=qD�}qD۽qD��qD�=qD�}qDܽqD��qD�=qD�}qDݽqD��qD�=qD�}qD޽qD��qD�=qD�}qD߽qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD��qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�@�D�w11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�A�1A�
=A��A��A��A��A� �A�"�A�"�A�"�A� �A� �A� �A�"�A� �A��A�A���A�x�A���A�^5A��TA�bA�bA���A��+A��A�XA��DA���A�(�A��mA��A��
A��-A�l�A�-A��A�/A��TA���A��^A�oA���A�ĜA�(�A�;dA�~�A�&�A���A�E�A�bNA���A�ffA��^A��hA�ȴA���A�A�bNA�+A��TA��A���A���A���A���A�;dA���A��RA�-A���A�M�A�hA}XA|�!A|Az�DAw��Aq��Ao�7Am��Al��AjE�Ai�AhffAg�PAgoAf��Ae�Ad�!Acp�Ab�Ab�!Ab$�A`��A_p�A^��A]t�A\�+AZ��AY��AV��AU"�AT�AS��AS�AR�ARI�AQ�AQoAOS�AN5?AM��AM+AKK�AJ9XAJA�AJA�AJ9XAIl�AG\)AFffAE��AD�9AD �AC��AC"�ABffAA�;A@�/A?��A?;dA>VA=�hA=?}A<�`A<I�A;�A:�A9��A9\)A8I�A7��A7�A7hsA7K�A6�A6��A5�TA5\)A4�A4A�A3�PA2E�A1oA0ĜA0{A/C�A.��A.��A.~�A.=qA-C�A+�;A+�-A+|�A+�A*�RA)�A)�PA(�/A(1'A'oA%�FA%|�A$�/A$Q�A$bA#��A#dZA#"�A"�+A!��A ��A ��A  �Ap�A+A�HA-A"�A1'A|�A�A��A{A\)AffAx�A��A$�AAS�AVA��A�hA�jAbAK�A��AA�A��A�-AO�A�A�hAĜA�A�FA
�!A	\)A�^A�uA�A�^A&�A  A?}Ar�AdZA"�A �A 1@���@�&�@���@�ff@���@���@��u@�~�@�l�@��y@�n�@�E�@��T@�@�z�@�o@�-@�9@�Z@�|�@�^5@��@�Z@�w@㕁@�
=@�-@�b@��@�E�@ܬ@ۍP@�n�@�hs@��/@�bN@���@�33@���@���@�1'@Ӯ@�;d@���@с@�z�@�K�@·+@�?}@̓u@���@ʏ\@ɺ^@��@ȣ�@ȃ@�1@�ƨ@���@�1@�K�@\@��#@�r�@��R@��7@���@� �@���@�+@��\@���@��`@�r�@�|�@�$�@��-@��h@��@�z�@�  @�@�X@��@� �@��!@��@��@��@�I�@���@��H@���@�/@��@�Z@��@�\)@��@��`@���@�z�@�bN@���@�|�@��H@�5?@��@��D@� �@�l�@�
=@���@��#@���@�j@��m@��F@�o@���@�=q@�J@��@�O�@�7L@��@���@�b@�|�@��@��+@�{@���@��h@�hs@���@� �@���@�@�ȴ@�ff@��T@��^@���@���@�z�@�I�@�9X@��@���@�;d@�
=@�ȴ@�v�@�M�@�=q@�$�@���@�O�@�7L@�%@��j@�1'@���@�;d@�33@��@��@���@�v�@�v�@�^5@�@��^@���@��@�O�@�%@���@�  @�;d@�;d@���@��R@�V@�@��@�%@�Z@�I�@�r�@� �@�Q�@�9X@���@���@�ƨ@�ƨ@���@��
@��F@�\)@�
=@��y@���@�M�@��@���@��-@���@���@��#@�?}@��@�Z@�A�@�1@�@�  @� �@�r�@��D@��@�z�@�j@�I�@l�@~��@~�@~��@~ȴ@~�@~ȴ@~E�@}/@}p�@}`B@}V@}V@}V@|�@|j@{�@{dZ@z��@y�@yG�@vȴ@t�D@t�D@u�@uV@t(�@t1@s�
@r��@r=q@r�@rJ@q��@qhs@q%@q%@q�@q�@q%@p��@p�@pA�@p �@n�y@nE�@m�@mV@l�j@l�D@lj@lj@lZ@l(�@kdZ@j��@j~�@jM�@jM�@i�@i�7@i7L@h��@h1'@gl�@g
=@f��@f@e`B@d�j@d�@dj@c��@cƨ@cƨ@c��@cdZ@cC�@b�!@b��@c@cdZ@c�@c��@c�F@c�@cdZ@c"�@b=q@a�^@a7L@`r�@_��@_l�@_|�@_K�@_K�@_\)@^�@^v�@]�-@]�@]�h@]?}@\��@]?}@]?}@]/@\�/@\z�@\j@\(�@\1@[�m@[��@\1@[�m@[��@[33@[@Z�@Z�!@Z=q@Y�#@Y��@X��@XbN@Xb@W�P@W�@V��@V�y@V�@Vȴ@VV@V$�@U��@U`B@UO�@T��@T��@S�m@S��@St�@St�@St�@St�@SdZ@SdZ@So@R�!@R�\@R-@Q�^@Qhs@P��@PQ�@P �@O�@O�P@O|�@Ol�@O;d@O
=@N�R@N��@N�+@Nff@NV@M��@M@MO�@M�@L�@L�/@L�D@L9X@K�
@K��@K33@K"�@K@J��@J=q@J-@I��@I��@Ix�@IG�@H�u@H �@G�P@Gl�@G\)@GK�@G
=@Fȴ@F��@F5?@E��@EO�@D��@D�@D�@DZ@DI�@C��@Cƨ@C��@CS�@CC�@Co@B��@B�!@Bn�@BM�@B�@A�#@A��@A��@Ax�@A7L@A%@@��@@�u@@Q�@@1'@?��@?l�@?K�@?;d@?;d@?�@>�R@>�+@>v�@>V@>5?@>@=�-@=?}@<�@<�/@<�j@<z�@;�m@;�F@;��@;dZ@;dZ@;"�@:��@:�\@:^5@:=q@9��@9�#@9�^@9��@9hs@9&�@8�`@8��@8bN@8  @7��@7�P@7l�@6�R@6�+@6E�@5�T@5?}@5�@5V@4�@4�/@4�j@49X@3�
@3�F@3t�@3S�@3C�@2�@2�!@2n�@2=q@2J@1�#@1x�@1x�@1X@17L@1%@0�`@0Ĝ@0A�@/�w@/��@/�P@/�P@/\)@/
=@.�y@.��@.ff@-�T@-@-�h@-p�@-�@,��@,��@,j@,I�@,9X@,(�@,�@,1@+�F@+S�@*�H@*��@*��@*~�@*=q@*�@)�@)�7@)hs@)�@(�9@(A�@(b@'�w@';d@'
=@&�R@&V@&$�@%�-@%�h@%p�@%?}@%/@%�@$��@$I�@$9X@$9X@$�@#��@#�F@#��@#�@#33@#@"�!@"��@"=q@!��@!hs@!X@!G�@!G�@!�@!%@ �`@ ��@ bN@   @�;@��@��@�w@�P@;d@ȴ@�+@ff@ff@V@E�@$�@@p�@V@��@�D@(�@�m@ƨ@��@��@��@��@�@�@S�@"�@�@�H@n�@J@�#@�#@�#@x�@�@��@��@Ĝ@Ĝ@�9@��@�u@bN@ �@�;@�w@�P@\)@\)@\)@K�@K�@;d@�@
=@�@�+@{@��@�h@�@p�@?}@V@�/@�@z�@9X@�
@��@��@��@��@��@�@t�@S�@@�!@��@~�@M�@M�@-@�@��@�^@X@G�@G�@X@X@7L@�@��@��@Ĝ@��@r�@A�@1'@1'@1'@ �@�@��@��@��@�P@|�@;d@
=@��@��@�@ȴ@�R@�R@�+@E�@�T@@��@�h@p�@O�@O�@?}@V@�/@�/@�/@�/@�@j@9X@��@�F@��@t�@S�@o@
�@
��@
��@
��@
��@
�\@
=q@
J@	�@	��@	�7@	hs@	G�@	&�@	�@	%@	%@��@�`@�9@�u@�u@�u11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�A�1A�
=A��A��A��A��A� �A�"�A�"�A�"�A� �A� �A� �A�"�A� �A��A�A���A�x�A���A�^5A��TA�bA�bA���A��+A��A�XA��DA���A�(�A��mA��A��
A��-A�l�A�-A��A�/A��TA���A��^A�oA���A�ĜA�(�A�;dA�~�A�&�A���A�E�A�bNA���A�ffA��^A��hA�ȴA���A�A�bNA�+A��TA��A���A���A���A���A�;dA���A��RA�-A���A�M�A�hA}XA|�!A|Az�DAw��Aq��Ao�7Am��Al��AjE�Ai�AhffAg�PAgoAf��Ae�Ad�!Acp�Ab�Ab�!Ab$�A`��A_p�A^��A]t�A\�+AZ��AY��AV��AU"�AT�AS��AS�AR�ARI�AQ�AQoAOS�AN5?AM��AM+AKK�AJ9XAJA�AJA�AJ9XAIl�AG\)AFffAE��AD�9AD �AC��AC"�ABffAA�;A@�/A?��A?;dA>VA=�hA=?}A<�`A<I�A;�A:�A9��A9\)A8I�A7��A7�A7hsA7K�A6�A6��A5�TA5\)A4�A4A�A3�PA2E�A1oA0ĜA0{A/C�A.��A.��A.~�A.=qA-C�A+�;A+�-A+|�A+�A*�RA)�A)�PA(�/A(1'A'oA%�FA%|�A$�/A$Q�A$bA#��A#dZA#"�A"�+A!��A ��A ��A  �Ap�A+A�HA-A"�A1'A|�A�A��A{A\)AffAx�A��A$�AAS�AVA��A�hA�jAbAK�A��AA�A��A�-AO�A�A�hAĜA�A�FA
�!A	\)A�^A�uA�A�^A&�A  A?}Ar�AdZA"�A �A 1@���@�&�@���@�ff@���@���@��u@�~�@�l�@��y@�n�@�E�@��T@�@�z�@�o@�-@�9@�Z@�|�@�^5@��@�Z@�w@㕁@�
=@�-@�b@��@�E�@ܬ@ۍP@�n�@�hs@��/@�bN@���@�33@���@���@�1'@Ӯ@�;d@���@с@�z�@�K�@·+@�?}@̓u@���@ʏ\@ɺ^@��@ȣ�@ȃ@�1@�ƨ@���@�1@�K�@\@��#@�r�@��R@��7@���@� �@���@�+@��\@���@��`@�r�@�|�@�$�@��-@��h@��@�z�@�  @�@�X@��@� �@��!@��@��@��@�I�@���@��H@���@�/@��@�Z@��@�\)@��@��`@���@�z�@�bN@���@�|�@��H@�5?@��@��D@� �@�l�@�
=@���@��#@���@�j@��m@��F@�o@���@�=q@�J@��@�O�@�7L@��@���@�b@�|�@��@��+@�{@���@��h@�hs@���@� �@���@�@�ȴ@�ff@��T@��^@���@���@�z�@�I�@�9X@��@���@�;d@�
=@�ȴ@�v�@�M�@�=q@�$�@���@�O�@�7L@�%@��j@�1'@���@�;d@�33@��@��@���@�v�@�v�@�^5@�@��^@���@��@�O�@�%@���@�  @�;d@�;d@���@��R@�V@�@��@�%@�Z@�I�@�r�@� �@�Q�@�9X@���@���@�ƨ@�ƨ@���@��
@��F@�\)@�
=@��y@���@�M�@��@���@��-@���@���@��#@�?}@��@�Z@�A�@�1@�@�  @� �@�r�@��D@��@�z�@�j@�I�@l�@~��@~�@~��@~ȴ@~�@~ȴ@~E�@}/@}p�@}`B@}V@}V@}V@|�@|j@{�@{dZ@z��@y�@yG�@vȴ@t�D@t�D@u�@uV@t(�@t1@s�
@r��@r=q@r�@rJ@q��@qhs@q%@q%@q�@q�@q%@p��@p�@pA�@p �@n�y@nE�@m�@mV@l�j@l�D@lj@lj@lZ@l(�@kdZ@j��@j~�@jM�@jM�@i�@i�7@i7L@h��@h1'@gl�@g
=@f��@f@e`B@d�j@d�@dj@c��@cƨ@cƨ@c��@cdZ@cC�@b�!@b��@c@cdZ@c�@c��@c�F@c�@cdZ@c"�@b=q@a�^@a7L@`r�@_��@_l�@_|�@_K�@_K�@_\)@^�@^v�@]�-@]�@]�h@]?}@\��@]?}@]?}@]/@\�/@\z�@\j@\(�@\1@[�m@[��@\1@[�m@[��@[33@[@Z�@Z�!@Z=q@Y�#@Y��@X��@XbN@Xb@W�P@W�@V��@V�y@V�@Vȴ@VV@V$�@U��@U`B@UO�@T��@T��@S�m@S��@St�@St�@St�@St�@SdZ@SdZ@So@R�!@R�\@R-@Q�^@Qhs@P��@PQ�@P �@O�@O�P@O|�@Ol�@O;d@O
=@N�R@N��@N�+@Nff@NV@M��@M@MO�@M�@L�@L�/@L�D@L9X@K�
@K��@K33@K"�@K@J��@J=q@J-@I��@I��@Ix�@IG�@H�u@H �@G�P@Gl�@G\)@GK�@G
=@Fȴ@F��@F5?@E��@EO�@D��@D�@D�@DZ@DI�@C��@Cƨ@C��@CS�@CC�@Co@B��@B�!@Bn�@BM�@B�@A�#@A��@A��@Ax�@A7L@A%@@��@@�u@@Q�@@1'@?��@?l�@?K�@?;d@?;d@?�@>�R@>�+@>v�@>V@>5?@>@=�-@=?}@<�@<�/@<�j@<z�@;�m@;�F@;��@;dZ@;dZ@;"�@:��@:�\@:^5@:=q@9��@9�#@9�^@9��@9hs@9&�@8�`@8��@8bN@8  @7��@7�P@7l�@6�R@6�+@6E�@5�T@5?}@5�@5V@4�@4�/@4�j@49X@3�
@3�F@3t�@3S�@3C�@2�@2�!@2n�@2=q@2J@1�#@1x�@1x�@1X@17L@1%@0�`@0Ĝ@0A�@/�w@/��@/�P@/�P@/\)@/
=@.�y@.��@.ff@-�T@-@-�h@-p�@-�@,��@,��@,j@,I�@,9X@,(�@,�@,1@+�F@+S�@*�H@*��@*��@*~�@*=q@*�@)�@)�7@)hs@)�@(�9@(A�@(b@'�w@';d@'
=@&�R@&V@&$�@%�-@%�h@%p�@%?}@%/@%�@$��@$I�@$9X@$9X@$�@#��@#�F@#��@#�@#33@#@"�!@"��@"=q@!��@!hs@!X@!G�@!G�@!�@!%@ �`@ ��@ bN@   @�;@��@��@�w@�P@;d@ȴ@�+@ff@ff@V@E�@$�@@p�@V@��@�D@(�@�m@ƨ@��@��@��@��@�@�@S�@"�@�@�H@n�@J@�#@�#@�#@x�@�@��@��@Ĝ@Ĝ@�9@��@�u@bN@ �@�;@�w@�P@\)@\)@\)@K�@K�@;d@�@
=@�@�+@{@��@�h@�@p�@?}@V@�/@�@z�@9X@�
@��@��@��@��@��@�@t�@S�@@�!@��@~�@M�@M�@-@�@��@�^@X@G�@G�@X@X@7L@�@��@��@Ĝ@��@r�@A�@1'@1'@1'@ �@�@��@��@��@�P@|�@;d@
=@��@��@�@ȴ@�R@�R@�+@E�@�T@@��@�h@p�@O�@O�@?}@V@�/@�/@�/@�/@�@j@9X@��@�F@��@t�@S�@o@
�@
��@
��@
��@
��@
�\@
=q@
J@	�@	��@	�7@	hs@	G�@	&�@	�@	%@	%@��@�`@�9@�u@�u@�u11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B
=B0!BL�BbNBbNBZBL�B<jB49B%�BDB��B�`B��B�B��B��Bv�BbNB^5BE�B<jB8RB9XB1'B'�B�B�BoBJBB��B�mB��B��BÖB�qB�3B��B��B��B�Bt�BgmBP�B8RB �BuBJB
��B
�yB
�)B
��B
�jB
��B
��B
{�B
N�B
;dB
5?B
-B
�B
B	��B	ÖB	�FB	�B	��B	��B	��B	��B	��B	�B	�B	�B	�B	��B	��B	��B	��B	��B	�\B	�1B	�B	w�B	p�B	^5B	T�B	N�B	J�B	F�B	C�B	@�B	<jB	8RB	/B	)�B	&�B	"�B	�B	�B	�B	�B	�B	�B	1B	B��B��B��B�B�B�B�B�sB�fB�ZB�BB�)B�B�B��B��B��B��B��BȴBǮBƨBŢBŢBĜBĜB��B�wB�dB�XB�?B�!B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�hB�\B�VB�JB�=B�7B�7B�7B�1B�%B�B�B� B~�B|�B|�Bz�Bw�Bu�Br�Bp�Bo�Bn�Bl�Bk�BhsBffBe`BcTBaHB`BB_;B]/B[#BYBW
BVBS�BR�BQ�BP�BO�BM�BL�BJ�BH�BF�BC�B@�B=qB<jB;dB:^B8RB6FB49B2-B/B.B-B.B,B+B)�B(�B&�B$�B"�B!�B!�B �B �B �B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B!�B!�B#�B#�B#�B#�B#�B#�B(�B)�B+B+B-B/B2-B33B49B5?B5?B6FB7LB7LB7LB9XB<jB=qB<jB?}B@�B@�BA�B@�BB�BB�BD�BH�BM�BO�BQ�BS�BT�BXBW
BW
BZB\)B^5B`BBbNBcTBcTBdZBgmBhsBiyBk�Bo�Bq�Br�Bu�Bv�By�B� B�B�B�7B�7B�JB�\B�hB�oB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�!B�3B�9B�LB�^B�^B�^B�}BÖBĜBĜBƨBɺB��B��B��B��B��B�B�
B�#B�/B�/B�;B�HB�`B�B�B�B�B�B�B��B��B��B��B��B	  B	B	B	+B		7B	DB	JB	\B	bB	hB	uB	�B	�B	�B	�B	�B	�B	�B	!�B	#�B	(�B	,B	,B	-B	-B	-B	/B	0!B	1'B	1'B	49B	6FB	8RB	:^B	<jB	?}B	B�B	D�B	E�B	F�B	H�B	H�B	I�B	J�B	L�B	O�B	T�B	W
B	YB	ZB	[#B	\)B	]/B	^5B	_;B	`BB	bNB	dZB	e`B	ffB	ffB	iyB	iyB	l�B	m�B	n�B	p�B	q�B	t�B	u�B	w�B	w�B	v�B	t�B	t�B	t�B	x�B	|�B	{�B	{�B	{�B	~�B	� B	�B	�B	�B	�B	�+B	�=B	�PB	�VB	�VB	�VB	�hB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�3B	�3B	�9B	�FB	�LB	�RB	�RB	�XB	�^B	�jB	�wB	��B	ĜB	ƨB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�)B	�/B	�/B	�5B	�BB	�BB	�BB	�HB	�NB	�TB	�ZB	�ZB	�`B	�`B	�fB	�sB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
%B
+B
	7B

=B

=B

=B

=B
DB
DB
JB
JB
PB
PB
VB
VB
VB
VB
VB
\B
\B
bB
bB
bB
bB
bB
hB
hB
hB
oB
oB
uB
uB
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
"�B
"�B
#�B
#�B
#�B
#�B
#�B
$�B
%�B
&�B
&�B
'�B
'�B
(�B
(�B
(�B
(�B
)�B
)�B
)�B
)�B
+B
+B
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
/B
/B
0!B
0!B
0!B
1'B
1'B
1'B
1'B
2-B
33B
33B
33B
49B
49B
49B
5?B
6FB
6FB
6FB
8RB
8RB
8RB
8RB
8RB
8RB
8RB
8RB
9XB
9XB
9XB
:^B
:^B
:^B
;dB
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
?}B
?}B
?}B
?}B
@�B
@�B
A�B
A�B
A�B
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
E�B
E�B
E�B
F�B
F�B
F�B
F�B
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
P�B
P�B
P�B
P�B
Q�B
Q�B
Q�B
R�B
R�B
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
W
B
W
B
XB
XB
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
[#B
[#B
[#B
[#B
\)B
]/B
]/B
]/B
]/B
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
ffB
ffB
ffB
ffB
ffB
ffB
ffB
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
jB
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
k�B
k�B
k�B
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
m�B
m�B
m�B
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
p�B
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
r�B
r�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�\BGB0ZBL�BcvBc�B\ABN�B=�B7�B*�B�B��B��B�CB��B��B�vBzKBb�Bb�BHB@�B:�B<B3B)LB �B�B&B�BB��B�uBԧB�AB�*B�B��B�zB��B�mB��Bv�BkBT	B;BB";BB�B.B
�B
�B
дB
��B
�B
�bB
��B
OiB
;�B
6�B
/�B
#�B
YB	ӺB	űB	��B	�7B	�YB	�xB	��B	�LB	��B	�]B	��B	�eB	�4B	�\B	�B	��B	��B	�KB	�B	��B	��B	ySB	u B	_�B	U�B	O&B	K{B	G@B	D?B	A�B	=�B	:�B	08B	*�B	(=B	%�B	VB	�B	�B	(B	}B	B		B	�B�sB�PB�\B��B�B�`B��B�B�^B�B�BܩB��B�B�B��B�8BΑB�AB�B�B��B��B�KB�4BŤB�#B�7B�XB�~B�B�hB�B�B��B�AB�B�2B��B��B��B�eB�9B�sB�qB��B�3B��B��B�IB��B��B�>B��B�pB��B��B��B�[B�hB��B��B��B�B}6B}�B|+BydBv�Bs�Bq&Bp;Bo�Bm�Bl�Bi�BgBf@Bc�Ba�B`�B`7B^�B\@BY�BXBV�BT^BS^BR{BQ�BQ�BN�BM�BK�BIgBHhBE�BB�B>vB<�B<B;B:)B7 B5fB3lB/B.�B.�B/B-B,B*�B)�B(�B'wB#�B#�B"-B!B �B!/B 6B �B�B@B�BB}B�B�BB	B�BXB�B�B,BXB�B_BjB=B�B�BBbB�B3BBBB+B�BqB�B3B�B 3B SB �B"-B"GB$B$B$\B$8B$�B%�B(�B*�B+�B,B.>B/�B2|B3�B4�B5�B5�B7B7�B7�B8;B:CB<�B=�B<�B@B@�BA\BB�B@�BCBC�BEBH�BN<BP�BR�BT�BU�BX0BWNBW�BZ|B\�B_@B`�BbKBclBc�Bd�Bg�BiBjBlXBpBrBsGBvBw4Bz�B��B�6B��B�|B��B��B��B��B��B��B��B��B��B�>B��B��B�(B�B�B�#B�*B�tB�UB�|B��B�dB��B��B�jB��B��B��BèBĳB��B��B�B��B�B�B�B�B�8B�{B�RB�BB�rBߑB�B��B�B�B�B��B��B��B��B��B� B�B�B	 ,B	=B	\B	tB		�B	�B	1B	�B	�B	�B	�B	�B	kB	�B	�B	�B	B	�B	!�B	$B	)B	,	B	,B	-B	- B	-MB	/wB	0`B	1MB	1|B	4qB	6sB	8�B	:mB	<oB	?gB	B�B	E+B	FB	F�B	H�B	H�B	I�B	J�B	L�B	O�B	UB	W%B	Y+B	Z>B	[[B	\�B	];B	^=B	_MB	`=B	b[B	d�B	e�B	f�B	f7B	i�B	i�B	l�B	m�B	n�B	p�B	r
B	t�B	v0B	x'B	x?B	w�B	uUB	tcB	t�B	y
B	}8B	{�B	|B	|hB	'B	�B	�B	�GB	�-B	�?B	�*B	�BB	�^B	�kB	�xB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	�B	�B	�B	�B	�:B	�<B	�"B	�=B	�`B	�ZB	�ZB	�6B	�XB	�lB	�QB	�VB	�oB	�zB	�pB	��B	�NB	�nB	�iB	ĬB	ƯB	ȻB	��B	��B	��B	�2B	�B	�B	�+B	�B	�B	��B	�!B	��B	�	B	�AB	�AB	�lB	�B	�0B	�[B	�CB	�B	�YB	�ZB	�rB	�sB	�SB	�yB	�mB	�kB	�\B	�jB	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	�B	��B	��B	�B	��B	��B	��B	��B	��B	�B	�B	��B	�$B	��B	�kB	�B
 B
B
B
B
B
B
BB
=B
*B
MB
TB
SB
�B
	UB

]B

vB

DB

MB
YB
iB
gB
sB
_B
jB
pB
nB
�B
qB
�B
lB
uB
vB
�B
�B
�B
�B
�B
tB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
"�B
"�B
#�B
#�B
#�B
#�B
#�B
$�B
&B
'B
' B
(+B
(B
)B
)B
)B
)B
*%B
*B
*
B
*B
+B
+'B
*,B
+4B
,)B
,B
,&B
-<B
-QB
-"B
-$B
.5B
.B
.CB
.@B
/1B
/6B
/4B
0CB
04B
0:B
1@B
1GB
1IB
1LB
2TB
3WB
3cB
3^B
4@B
4_B
4�B
5\B
6qB
6~B
6�B
8^B
8cB
8iB
8gB
8wB
8�B
8zB
8bB
9~B
9lB
9oB
:�B
:|B
:B
;�B
;�B
;�B
;�B
<pB
<�B
=�B
=�B
=�B
=�B
=�B
>�B
?�B
?�B
?�B
?�B
@�B
@�B
A�B
A�B
A�B
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
E�B
E�B
E�B
F�B
F�B
F�B
F�B
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
O B
N�B
O�B
O�B
O�B
O�B
PB
P�B
QB
QB
P�B
RB
R	B
R+B
S2B
S B
TB
T	B
TB
TB
TB
TB
TB
T>B
U"B
UB
UB
UB
UB
U!B
V/B
V6B
WB
WB
WB
WB
W B
W)B
W@B
X<B
X@B
YHB
Z$B
ZNB
Z7B
Z+B
Z1B
[-B
[3B
[-B
[9B
[.B
[DB
[@B
[?B
[:B
[bB
\QB
]BB
]5B
]AB
]eB
]UB
^MB
_<B
_KB
_HB
_PB
_PB
_PB
_]B
_`B
`aB
`TB
`]B
aaB
aLB
aUB
a\B
aRB
a\B
aaB
bcB
btB
bxB
b�B
cmB
cpB
dfB
dpB
d}B
dtB
dwB
dwB
e�B
e�B
e�B
f~B
flB
foB
fqB
ftB
fwB
fxB
f�B
f�B
f�B
gyB
g�B
g�B
gqB
g�B
g�B
g�B
g�B
h�B
hzB
hxB
hvB
h�B
h�B
h�B
h�B
h|B
h�B
i�B
i�B
i�B
i�B
i�B
i�B
i�B
i�B
j�B
j�B
j�B
j�B
j�B
j�B
j�B
j�B
j�B
j�B
k�B
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
p�B
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
r�B
r�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<*k!<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - (REPORTED_SURFACE_PRESSURE) for PADJ                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            PSAL_ADJ corrects Cnd. Thermal Mass (CTM), Johnson et al,2007,JAOT & effects of pressure adjustments                                                                                                                                                            PADJ REPORTED_SURFACE_PRESSURE =0.08 dbar                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            CTM alpha = 0.0267 & tau = 18.6 s with error equal to the correction                                                                                                                                                                                            Pressures adjusted using reported surface pressure when warranted.                                                                                                                                                                                              none                                                                                                                                                                                                                                                            Salinity corrected for pressure sensor calibration drift and cell thermal mass                                                                                                                                                                                  202307121057282023071210572820230712105728  AO  ARCAADJP                                                                    20181206100029    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181206100029  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181206100029  QCF$                G�O�G�O�G�O�0               PM  ARSQPADJV1.1                                                                20230712105728  QC  PRES            @�33D�y�G�O�                PM  ARSQCTM V1.1                                                                20230712105728  QC  PSAL            @�33D�y�G�O�                PM  ARSQCOWGV1.1CTD_2021v2 + Argo_2021v03                                       20230721230919  IP                  G�O�G�O�G�O�                