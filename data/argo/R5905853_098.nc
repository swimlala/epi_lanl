CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T17:40:15Z creation;2022-06-04T17:40:16Z conversion to V3.1      
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
_FillValue                 �  I<   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M,   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �@   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �0   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �D   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �    	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ޼   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20220604174015  20220610131507  5905853                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               bA   JA                                  2B  A   APEX                            8421                            2.11.2                          846 @ٚ��r1   @ٚ�J�͏@/k��Q��cC��$�1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @,��@�  @�  @���AffA>ffA`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8ffB@ffBG��BP  BX  B`  Bh  Bp  Bx  B�33B�  B�  B�  B�  B�  B���B�  B�  B�  B���B�ffB�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  C   C  C�fC  C  C
  C  C�C�C  C  C  C  C�fC  C  C   C"  C#�fC&  C(  C*  C,  C.  C0  C2  C4  C6  C8�C:�C<  C>  C@�CA�fCD  CE�fCH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Ce�fCh  Cj  Cl  Cn  Cp  Cr�CtL�Cv  Cw�fCz  C|  C~  C�fC�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�C3DÃ3D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D��3D�3D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؃3D��3D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @'�@z�H@�p�@�=qA�A=�A^�RA~�RA�\)A�\)A�\)A�\)A�\)A�\)A�\)A�\)B�B�B�B�B'�B/�B8zB@zBGG�BO�BW�B_�Bg�Bo�Bw�B�
=B��
B��
B��
B��
B��
B���B��
B��
B��
B�p�B�=pB��
B��
B��
B��
B��
B��
B��
B��
Bϣ�B��
B��
Bۣ�B��
B��
B��
B��
B��
B��
B��
B��
B��
C�C��C�C�C	�C�CCC�C�C�C�C��C�C�C�C!�C#��C%�C'�C)�C+�C-�C/�C1�C3�C5�C8C:C;�C=�C@CA��CC�CE��CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce��Cg�Ci�Ck�Cm�Co�CrCt8RCu�Cw��Cy�C{�C}�C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D z�D ��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��D	z�D	��D
z�D
��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�DGDz�D��Dz�D��Dz�D��Dz�D��Dz�D��D z�D ��D!z�D!��D"z�D"��D#z�D#��D$z�D$��D%z�D%��D&z�D&��D'z�D'��D(z�D(��D)z�D)��D*z�D*��D+z�D+��D,z�D,��D-z�D-��D.z�D.��D/z�D/��D0z�D0��D1z�D1��D2z�D2��D3z�D3��D4z�D4��D5z�D5��D6z�D6��D7z�D7��D8z�D8��D9z�D9��D:z�D:��D;z�D;��D<z�D<��D=z�D=��D>z�D>��D?z�D?��D@z�D@��DAz�DA��DBz�DB��DCz�DC��DDz�DD��DEz�DE��DFz�DF��DGz�DG��DHz�DH��DIz�DI��DJz�DJ��DKz�DK��DLz�DL��DMz�DM��DNz�DN��DOz�DO��DPz�DP��DQz�DQ��DRz�DR��DSz�DS��DTz�DT��DUz�DU��DVz�DV��DWz�DW��DXz�DX��DYz�DY��DZz�DZ��D[z�D[��D\z�D\��D]z�D]��D^z�D^��D_z�D_��D`z�D`��Daz�Da��Dbz�Db��Dcz�Dc��Ddz�Dd��Dez�De��Dfz�Df��Dgz�Dg��Dhz�Dh��Diz�Di��Djz�Dj��Dkz�Dk��Dlz�Dl��Dmz�Dm��Dnz�Dn��Doz�Do��Dpz�Dp��Dqz�Dq��Drz�Dr��Dsz�Ds��Dtz�Dt��Duz�Du��Dvz�Dv��Dwz�Dw��Dxz�Dx��Dyz�Dy��Dzz�Dz��D{z�D{��D|z�D|��D}z�D}��D~z�D~��Dz�D��D�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�@�D�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD½qD��qD�@�DÀ�DýqD��qD�=qD�}qDĽqD��qD�=qD�}qDŽqD��qD�=qD�}qDƽqD��qD�=qD�}qDǽqD��qD�=qD�}qDȽqD��qD�=qD�}qDɽqD��qD�=qD�}qDʽqD��qD�=qD�}qD˽qD��qD�=qD�}qD̽qD��qD�=qD�}qD���D� �D�=qD�}qDνqD��qD�=qD�}qDϽqD��qD�=qD�}qDнqD��qD�=qD�}qDѽqD��qD�=qD�}qDҽqD��qD�=qD�}qDӽqD��qD�=qD�}qDԽqD��qD�=qD�}qDսqD��qD�=qD�}qDֽqD��qD�=qD�}qD׽qD��qD�=qD؀�D���D��qD�=qD�}qDٽqD��qD�=qD�}qDڽqD��qD�=qD�}qD۽qD��qD�=qD�}qDܽqD��qD�=qD�}qDݽqD��qD�=qD�}qD޽qD��qD�=qD�}qD߽qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD��qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��q11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A��OA��?A��tA��?A��XA��6A���A�͟A��A�бA���A�ԕA�՛A���A���A��KA��A��EA���A��jA���A���A��'Aӽ�AӽqAӿ}A���AӼjAӻ�AӽAӴ�Aӯ�AӲ�AӨ�Aә1AәeA�/OA�<6A�zA˅�A��A��Aě�A�[WA�_;A�o5A���A�-A��A���A� �A��6A��A��A�NA��A��A���A��dA�2�A��4A��A���A�A�sMA��gA�f2A�xA���A��A��wA�D�A�CaA��A�˒A�/�A��[A��BA���A��HA�H�A�'�A�Z�A��A���A�HKA���A���A�|�A���A��A���A��hA��HA��A��A���A{OAx��At��Aq�AoߤAkAg�Af��Ad�YAa�]A_�PA\��AZ�AW\�AUhsAS/�AP��AN�/AM�rAJ33AG��AF8AET�AC�uAB�cA@�A<|�A8�9A6$tA4�A3*0A/�EA/�A.}�A,VmA+?}A(MA$!-A"��A"�A!�)A �AI�A@OA�eA��A�yA��AK�A�kA|�A{JA��A�AoA�{A�pA��Al�Au%AS&A�KA�AJ�A�jA:*A��Al�A
=AɆA�A�AC-A)�A�Av`A��A��A�AA�xAA�A�dA�oA�A^5A�A��A�Au%Al�AA A�/A�OA��AA�^A�A_A	��A	˒A��A:*A��A�A/�A�rA�/A��A��A��A,�AFAuA�?A��ASAɆA�FAQ�A-�A�Al�A �KA Mj@���@�@�m�@���@��{@�!�@�!�@�4�@��@���@�	l@�c�@���@���@���@�J#@���@�c @���@�|@�O@���@��b@�@�@���@�"h@��@�j@��B@���@�r�@킪@��@��@�h@�{J@�B�@�u�@�:@�?}@���@�@�M@��m@��@�͟@�B[@�-@�u@��@��@吗@�V�@�,�@�J�@��@�"@��c@��@��H@��@�z@�H@߰�@� i@�A�@���@ݗ�@�G�@ܠ�@��@�x@��@ڢ4@ڂA@��@�B�@���@�]d@��@�C@���@ՙ�@Չ7@�W?@��@԰�@�Xy@��@�C@��@�]�@��@��)@�_�@��Q@�e�@�IR@�IR@ΦL@��j@�9�@�u�@�@�)_@ʌ@�C�@�'R@�ƨ@�a�@�/�@��@Ȁ�@��@�)_@�S�@��@ű[@��@��X@ĆY@��}@�	l@@�H�@�4@���@�2a@�q@��h@�h
@�V�@�$�@���@��=@�o @�33@��@�8�@��r@��H@�{J@�Q�@��@��@��?@���@�bN@��@��@�p;@��f@�O@�!�@���@��E@���@���@�7�@�v`@�@��"@���@�y>@�#:@�_@���@�g�@��"@���@���@�($@��=@�Mj@�+@��6@�˒@�|@��@�q@�C-@���@��n@�S�@��j@�N�@�3�@�	@��$@�8@�I�@�@O@���@�{@��A@��}@�C�@���@�u%@�q�@�-@���@�&@��P@���@�bN@�7�@�{@��:@��@��1@�:*@��g@�o @�	l@���@�~�@��@��K@��@@�U�@�o@���@�q�@�x@���@�[W@�҉@�]d@��@���@�N<@��@���@���@�J�@��7@�8@��@��@��@���@�C�@��g@���@�dZ@�,�@�]d@�"h@�Z�@�1�@��@���@��@��M@�p;@�	@��)@���@��C@�O�@��@��9@��@��Q@��@�خ@��@��:@��c@��O@���@��@�[�@�B[@�6�@�.�@�)�@��@���@�5�@�+�@�&@���@�~�@�:*@��@��&@���@���@���@�j�@�@O@��	@�� @�@���@�W?@��@��@���@���@��@�s�@�PH@�-�@��@��z@��'@�b�@�1�@�;@���@�e�@�GE@�-�@�4@��r@��@�خ@��N@���@��n@�X�@�4�@���@��F@�A�@���@�j@�L�@��@��D@�bN@��#@�[W@��@���@�H@�&�@��@���@��&@�F�@��@���@�%�@���@��@��}@��.@�q@�Q@�$�@�A@iD@~�@~ff@~�@}u�@|c�@{�W@{O@z^5@z#:@y��@yf�@x��@x��@xu�@x@w��@v�"@v�}@vc @u�>@u��@uDg@tѷ@t�O@tG@s�@s'�@r��@r�@rl�@r_@q�@q��@q��@q \@pѷ@p9X@o��@o]�@oC@n�c@n�1@n^5@ne@m�3@m^�@m@l��@l'R@k�K@k�@k1�@j�h@jq�@j1�@iϫ@ip�@h�5@h�O@h��@h_@hb@g�	@g�@f��@f�@e��@e/@d�I@dFt@c��@c�$@cMj@b�@bff@a��@a[W@a	l@`��@`Ɇ@`Q�@_|�@^�@^��@^s�@^@�@]��@]-w@]�@\�/@\e�@[��@[C�@Z͟@Z�@Zc @Z	@Yx�@X�@X�@W��@W�P@WE9@V�2@Vq�@V$�@U�^@Ue,@UV@TZ@S�:@R��@R!�@Q�d@Q`B@P�Y@PD�@O��@O�$@N�B@N��@NOv@M��@Mk�@M@L�I@L*�@L1@K�&@K��@KZ�@J��@J�r@JGE@J
�@I��@IT�@I@H�)@Hj@Hx@G��@GP�@F��@F�@FZ�@F�@E��@E(�@Dѷ@DQ�@D  @C��@C��@C�4@C;d@B�2@B��@B~�@B&�@A��@A�@A�X@A��@AN<@@�@@�z@@u�@@6@?�@?��@?�@>��@>� @>_@=�@=�^@=-w@<ی@<��@<M@;t�@;4�@:��@:��@:Q@9��@9�^@9(�@8�P@8�@8֡@8�Y@8$@7��@7��@7$t@6�H@6q�@61�@6$�@64@5��@5�S@5/@4��@4��@4S�@4�@3�@3��@3$t@3�@2�h@2c @2B[@1��@1��@17L@0�)@0��@0�.@0g8@0(�@/�@/��@/O@/ i@.��@-��@-�'@-Q�@-�@-%@,��@,�@,U2@+�@+��@+_p@*��@)�.@)�@)+@(�v@(�E@(��@(r�@(�@'�m@'��@'��@'�@&�}@&s�@&\�@&Z�@&?@& �@%��@%@@$�@$u�@$"h@#�K@#S�@#8@#o@"�,@"��@"��@"E�@!�-@!*0@!�@!+@ �|@ ��@ Ɇ@ ��@ �4@ ��@ �o@ bN@�A@�$@�	@t�@Y@�@E�@�3@�~@zx@J�@�@��@Ĝ@�_@_@A�@%�@��@��@_p@)_@�@�]@��@�@��@v�@=q@J@�>@�@o @^�@N<@V@�5@�$@m�@D�@<�@"h@1@�6@�f@v`@e�@K�@�@��@��@xl@.�@�@��@�Z@�@�C@o @�P@��@��@|�@Z@`�@PH@"h@�&@��@v`@+@��@s�@Z�@5?@5?@5?@0U@J@�H@�C@��@��@��@}�@a�@<6@��@Ɇ@�@oi@6@�@�]@�w@��@n/@Mj@S@�@�@��@Z�@�@�N@��@w2@J�@��@Ĝ@�U@�$@�u@*�@��@��@|�@l�@o�@F�@+@�@
��@
�@
i�@
8�@
!�@	�.@	��@	�@	��@	j@	2a@��@��@Ɇ@��@tT@�@�6@��@��@�	@|�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A��OA��?A��tA��?A��XA��6A���A�͟A��A�бA���A�ԕA�՛A���A���A��KA��A��EA���A��jA���A���A��'Aӽ�AӽqAӿ}A���AӼjAӻ�AӽAӴ�Aӯ�AӲ�AӨ�Aә1AәeA�/OA�<6A�zA˅�A��A��Aě�A�[WA�_;A�o5A���A�-A��A���A� �A��6A��A��A�NA��A��A���A��dA�2�A��4A��A���A�A�sMA��gA�f2A�xA���A��A��wA�D�A�CaA��A�˒A�/�A��[A��BA���A��HA�H�A�'�A�Z�A��A���A�HKA���A���A�|�A���A��A���A��hA��HA��A��A���A{OAx��At��Aq�AoߤAkAg�Af��Ad�YAa�]A_�PA\��AZ�AW\�AUhsAS/�AP��AN�/AM�rAJ33AG��AF8AET�AC�uAB�cA@�A<|�A8�9A6$tA4�A3*0A/�EA/�A.}�A,VmA+?}A(MA$!-A"��A"�A!�)A �AI�A@OA�eA��A�yA��AK�A�kA|�A{JA��A�AoA�{A�pA��Al�Au%AS&A�KA�AJ�A�jA:*A��Al�A
=AɆA�A�AC-A)�A�Av`A��A��A�AA�xAA�A�dA�oA�A^5A�A��A�Au%Al�AA A�/A�OA��AA�^A�A_A	��A	˒A��A:*A��A�A/�A�rA�/A��A��A��A,�AFAuA�?A��ASAɆA�FAQ�A-�A�Al�A �KA Mj@���@�@�m�@���@��{@�!�@�!�@�4�@��@���@�	l@�c�@���@���@���@�J#@���@�c @���@�|@�O@���@��b@�@�@���@�"h@��@�j@��B@���@�r�@킪@��@��@�h@�{J@�B�@�u�@�:@�?}@���@�@�M@��m@��@�͟@�B[@�-@�u@��@��@吗@�V�@�,�@�J�@��@�"@��c@��@��H@��@�z@�H@߰�@� i@�A�@���@ݗ�@�G�@ܠ�@��@�x@��@ڢ4@ڂA@��@�B�@���@�]d@��@�C@���@ՙ�@Չ7@�W?@��@԰�@�Xy@��@�C@��@�]�@��@��)@�_�@��Q@�e�@�IR@�IR@ΦL@��j@�9�@�u�@�@�)_@ʌ@�C�@�'R@�ƨ@�a�@�/�@��@Ȁ�@��@�)_@�S�@��@ű[@��@��X@ĆY@��}@�	l@@�H�@�4@���@�2a@�q@��h@�h
@�V�@�$�@���@��=@�o @�33@��@�8�@��r@��H@�{J@�Q�@��@��@��?@���@�bN@��@��@�p;@��f@�O@�!�@���@��E@���@���@�7�@�v`@�@��"@���@�y>@�#:@�_@���@�g�@��"@���@���@�($@��=@�Mj@�+@��6@�˒@�|@��@�q@�C-@���@��n@�S�@��j@�N�@�3�@�	@��$@�8@�I�@�@O@���@�{@��A@��}@�C�@���@�u%@�q�@�-@���@�&@��P@���@�bN@�7�@�{@��:@��@��1@�:*@��g@�o @�	l@���@�~�@��@��K@��@@�U�@�o@���@�q�@�x@���@�[W@�҉@�]d@��@���@�N<@��@���@���@�J�@��7@�8@��@��@��@���@�C�@��g@���@�dZ@�,�@�]d@�"h@�Z�@�1�@��@���@��@��M@�p;@�	@��)@���@��C@�O�@��@��9@��@��Q@��@�خ@��@��:@��c@��O@���@��@�[�@�B[@�6�@�.�@�)�@��@���@�5�@�+�@�&@���@�~�@�:*@��@��&@���@���@���@�j�@�@O@��	@�� @�@���@�W?@��@��@���@���@��@�s�@�PH@�-�@��@��z@��'@�b�@�1�@�;@���@�e�@�GE@�-�@�4@��r@��@�خ@��N@���@��n@�X�@�4�@���@��F@�A�@���@�j@�L�@��@��D@�bN@��#@�[W@��@���@�H@�&�@��@���@��&@�F�@��@���@�%�@���@��@��}@��.@�q@�Q@�$�@�A@iD@~�@~ff@~�@}u�@|c�@{�W@{O@z^5@z#:@y��@yf�@x��@x��@xu�@x@w��@v�"@v�}@vc @u�>@u��@uDg@tѷ@t�O@tG@s�@s'�@r��@r�@rl�@r_@q�@q��@q��@q \@pѷ@p9X@o��@o]�@oC@n�c@n�1@n^5@ne@m�3@m^�@m@l��@l'R@k�K@k�@k1�@j�h@jq�@j1�@iϫ@ip�@h�5@h�O@h��@h_@hb@g�	@g�@f��@f�@e��@e/@d�I@dFt@c��@c�$@cMj@b�@bff@a��@a[W@a	l@`��@`Ɇ@`Q�@_|�@^�@^��@^s�@^@�@]��@]-w@]�@\�/@\e�@[��@[C�@Z͟@Z�@Zc @Z	@Yx�@X�@X�@W��@W�P@WE9@V�2@Vq�@V$�@U�^@Ue,@UV@TZ@S�:@R��@R!�@Q�d@Q`B@P�Y@PD�@O��@O�$@N�B@N��@NOv@M��@Mk�@M@L�I@L*�@L1@K�&@K��@KZ�@J��@J�r@JGE@J
�@I��@IT�@I@H�)@Hj@Hx@G��@GP�@F��@F�@FZ�@F�@E��@E(�@Dѷ@DQ�@D  @C��@C��@C�4@C;d@B�2@B��@B~�@B&�@A��@A�@A�X@A��@AN<@@�@@�z@@u�@@6@?�@?��@?�@>��@>� @>_@=�@=�^@=-w@<ی@<��@<M@;t�@;4�@:��@:��@:Q@9��@9�^@9(�@8�P@8�@8֡@8�Y@8$@7��@7��@7$t@6�H@6q�@61�@6$�@64@5��@5�S@5/@4��@4��@4S�@4�@3�@3��@3$t@3�@2�h@2c @2B[@1��@1��@17L@0�)@0��@0�.@0g8@0(�@/�@/��@/O@/ i@.��@-��@-�'@-Q�@-�@-%@,��@,�@,U2@+�@+��@+_p@*��@)�.@)�@)+@(�v@(�E@(��@(r�@(�@'�m@'��@'��@'�@&�}@&s�@&\�@&Z�@&?@& �@%��@%@@$�@$u�@$"h@#�K@#S�@#8@#o@"�,@"��@"��@"E�@!�-@!*0@!�@!+@ �|@ ��@ Ɇ@ ��@ �4@ ��@ �o@ bN@�A@�$@�	@t�@Y@�@E�@�3@�~@zx@J�@�@��@Ĝ@�_@_@A�@%�@��@��@_p@)_@�@�]@��@�@��@v�@=q@J@�>@�@o @^�@N<@V@�5@�$@m�@D�@<�@"h@1@�6@�f@v`@e�@K�@�@��@��@xl@.�@�@��@�Z@�@�C@o @�P@��@��@|�@Z@`�@PH@"h@�&@��@v`@+@��@s�@Z�@5?@5?@5?@0U@J@�H@�C@��@��@��@}�@a�@<6@��@Ɇ@�@oi@6@�@�]@�w@��@n/@Mj@S@�@�@��@Z�@�@�N@��@w2@J�@��@Ĝ@�U@�$@�u@*�@��@��@|�@l�@o�@F�@+@�@
��@
�@
i�@
8�@
!�@	�.@	��@	�@	��@	j@	2a@��@��@Ɇ@��@tT@�@�6@��@��@�	@|�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B5?B5%B5B5%B5%B5B5B4�B4�B5B4�B5%B5B5ZB5%B5%B5%B5?B5B6`B7�B7�B88B8�B8�B8�B8�B9�B9�B:B<�B>�B?}BABC-BB�BE�B?B)�B
�LB
چB
�#B
ԯBB)_BM�B`\Bg�B�iB��B�!B�XB�XB��B��B��B�B�B�UB��B�wB��B�6B�	B��BʦB��B��B�B�'B�B��B��B��B�aBvFB}�B�%B.Br�Bk6B_�BT{BDgB=�B1B]B
�BGB
�B
��B
�B
�NB
��B
k�B
EmB
(>B	�%B	�HB	�~B	�^B	��B	��B	HB	tB	i�B	[�B	O(B	@�B	5�B	$�B	�B	0B	  B�zB�B��B�+B��B�B޸B�zB�2BߊB�B��B�jB��B��B�B	�B	!|B	#�B	$B	
rB	(B	�B	!HB	$�B	%B	-�B	/OB	1[B	2�B	9>B	J�B	fB	gmB	hXB	p�B	�B	��B	��B	��B	��B	�iB	��B	��B	�XB	ʦB	�(B	�(B	��B	�\B	��B	� B	�B	�;B	ܬB	��B	�KB	�B	�B	� B	��B	��B	��B	�]B	�B	�?B	��B	�jB	�B	�B	�$B	�2B	��B	��B	�AB	�B	��B	�+B	�vB	��B	�MB	�iB	��B	�B	�\B	ٚB	�B	�B	�SB	�YB	��B	�B	ںB	��B	�B	�B	��B	� B	��B	��B	��B	�B	�HB	��B	�hB	�B	�B	��B	�;B	޸B	�5B	ݲB	��B	�B	�bB	��B	�ZB	��B	�cB	�B	�B	�LB	�QB	�B	��B	�OB	�wB	�iB	��B	�B	��B	�aB	�TB	��B	��B	��B	��B	��B	��B	�B	�oB	�B	�B	��B	�B	�qB	�B	�QB	�B	�KB	�B	�yB	�B	�B	�B	��B	�/B	�B	�B	�B	�aB	�-B	�B	�;B	�!B	��B	��B	��B	�hB	�B	��B	��B	�nB	��B	�TB	�B	��B	�B	�?B	�tB	�tB	��B	��B	��B	��B	��B	�+B	�%B	��B	�B	��B	�	B	�*B	�DB	��B	��B	�B	�PB	�dB	�>B	�RB	��B	��B	�RB	��B	��B	�B	��B	�0B	�$B	��B	��B	��B	��B	��B	�B	��B	��B	�B	�B	�(B	��B	��B	��B	��B	��B	�6B	�(B	��B	�B	��B	�cB	��B	��B	��B
 B
 4B	��B
 B	��B	��B	��B	�HB	�B	�HB	�B	�HB	��B	��B	�wB	�]B	�wB	�wB	�BB	�]B	�(B	�(B	�BB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 4B
 B
 B
 �B
 �B
 �B
UB
�B
�B
�B
�B
�B
GB
aB
B
-B
�B
�B
aB
�B
�B
�B
�B
�B
�B
gB
B
�B
9B
�B
�B
MB
�B
�B
-B
B
�B
�B
 B
�B
�B
oB
 �B
 OB
  B
 B	��B	��B	��B	��B	��B	��B	�B	��B	��B	�}B	�}B	��B
  B
 B
 �B
 �B
 B
oB
[B
�B
�B
{B
B
�B
�B
�B
�B
YB
?B
B
?B
EB
�B
�B
�B
1B
fB
�B
�B
�B
	�B

�B

�B
�B
pB
BB
B
B
�B
bB
B
�B
�B
�B
9B
B
�B
�B
�B
$B
�B
�B
QB
�B
#B
�B
�B
�B
�B
IB
dB
~B
B
�B
B
�B
�B
B
�B
 BB
 \B
 �B
 �B
 �B
 �B
 �B
 �B
 �B
!B
!�B
!�B
"�B
"�B
"�B
"�B
"�B
#:B
#:B
#:B
#:B
#nB
#�B
$B
$ZB
$�B
$�B
$�B
%B
%,B
%B
%FB
%FB
%zB
%`B
%`B
%zB
%zB
%�B
%�B
%�B
%�B
%`B
&LB
&LB
&2B
&�B
&�B
&�B
'�B
'�B
(sB
(�B
)yB
*0B
+kB
+�B
+�B
-wB
-�B
-�B
.}B
/iB
/�B
/�B
/�B
/�B
0UB
0�B
1vB
1�B
2aB
2aB
2|B
2�B
3B
3hB
4TB
4�B
4�B
5%B
5?B
5�B
5�B
5�B
6B
6zB
6�B
6�B
72B
7�B
7�B
7�B
88B
88B
8�B
9$B
9�B
9�B
9�B
:*B
:^B
:xB
:^B
:�B
:�B
;B
;�B
;�B
;�B
<6B
<6B
<�B
<�B
<�B
<�B
="B
="B
=�B
=�B
=�B
>B
>(B
>wB
>�B
>�B
>�B
?B
?cB
?}B
?cB
?}B
?}B
@ B
@ B
@4B
@iB
@�B
@�B
AUB
A�B
A�B
A�B
A�B
A�B
B[B
B�B
B�B
B�B
B�B
B�B
B�B
C�B
D3B
D3B
D�B
D�B
D�B
E�B
E9B
ESB
E�B
FYB
F�B
F�B
F�B
F�B
F�B
GzB
G_B
GzB
G_B
G�B
GzB
HB
HB
H�B
H�B
IlB
I�B
I�B
I�B
J	B
J�B
J�B
KB
KxB
K^B
KDB
J�B
J�B
J�B
J�B
J�B
J#B
I�B
J#B
J#B
I�B
J	B
J#B
JXB
J�B
J�B
J�B
K)B
KDB
KxB
KxB
KxB
K�B
K�B
LB
L0B
L0B
L�B
MPB
M�B
N�B
N�B
N�B
OB
O�B
O�B
O�B
O�B
PbB
P�B
Q4B
QNB
Q�B
Q�B
Q�B
RB
RB
R:B
RTB
RoB
R�B
R�B
S[B
S�B
T,B
TFB
T,B
UB
T�B
T�B
U�B
U�B
VB
V�B
W?B
W�B
W�B
X+B
X_B
X�B
X�B
ZB
ZB
ZB
ZB
Z�B
[	B
[	B
[=B
[�B
[qB
[�B
[�B
[qB
[qB
[�B
[�B
[�B
\)B
\xB
\B
\]B
\)B
\xB
\�B
]B
]�B
]�B
^B
^�B
^�B
_VB
_�B
_�B
_�B
_�B
`B
`B
`BB
`�B
`vB
`�B
aB
a-B
a�B
a�B
a�B
a�B
a�B
a�B
bNB
b�B
b�B
c�B
c�B
c�B
d�B
d�B
d�B
e,B
e`B
ezB
e�B
e�B
fB
f�B
gRB
g�B
g�B
g�B
g�B
g�B
h
B
h>B
h�B
i�B
i�B
i�B
jKB
jeB
j�B
j�B
kB
k6B
kB
kkB
k�B
l=B
lqB
l�B
l�B
l�B
l�B
l�B
l�B
l�B
mB
m�B
m�B
m�B
m�B
n/B
n�B
o B
oiB
o�B
o�B
pB
pUB
p�B
p�B
p�B
qB
q'B
q'B
q�B
rB
rGB
raB
r�B
r�B
r�B
sB
s3B
sMB
s�B
s�B
s�B
tB
tnB
tTB
tTB
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
uZB
uZB
utB
utB
u�B
v+B
v+B
v�B
v�B
v�B
v�B
v�B
w2B
wfB
wLB
w�B
w�B
w�B
xB
x8B
xlB
x�B
x�B
y$B
yXB
y$B
y>B
y�B
y�B
z*B
z�B
z�B
z�B
{B
{dB
{�B
{�B
|B
|B
|B
|B
|B
|PB
|�B
|�B
|�B
}B
}<B
}<B
}qB
}�B
}�B
}�B
}�B
~]B
~]B
~�B
~�B
~�B
B
HB
.B
.B
HB
�B
�B
�B
�B
�iB
��B
��B
� B
��B
��B
��B
�B
��B
��B
�B
�AB
�'B
��B
��B
�B
�aB
�{B
�{B
��B
�MB
�MB
�gB
��B
��B
��B
��B
��B
��B
�B
��B
��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B5?B5%B5B5%B5%B5B5B4�B4�B5B4�B5%B5B5ZB5%B5%B5%B5?B5B6`B7�B7�B88B8�B8�B8�B8�B9�B9�B:B<�B>�B?}BABC-BB�BE�B?B)�B
�LB
چB
�#B
ԯBB)_BM�B`\Bg�B�iB��B�!B�XB�XB��B��B��B�B�B�UB��B�wB��B�6B�	B��BʦB��B��B�B�'B�B��B��B��B�aBvFB}�B�%B.Br�Bk6B_�BT{BDgB=�B1B]B
�BGB
�B
��B
�B
�NB
��B
k�B
EmB
(>B	�%B	�HB	�~B	�^B	��B	��B	HB	tB	i�B	[�B	O(B	@�B	5�B	$�B	�B	0B	  B�zB�B��B�+B��B�B޸B�zB�2BߊB�B��B�jB��B��B�B	�B	!|B	#�B	$B	
rB	(B	�B	!HB	$�B	%B	-�B	/OB	1[B	2�B	9>B	J�B	fB	gmB	hXB	p�B	�B	��B	��B	��B	��B	�iB	��B	��B	�XB	ʦB	�(B	�(B	��B	�\B	��B	� B	�B	�;B	ܬB	��B	�KB	�B	�B	� B	��B	��B	��B	�]B	�B	�?B	��B	�jB	�B	�B	�$B	�2B	��B	��B	�AB	�B	��B	�+B	�vB	��B	�MB	�iB	��B	�B	�\B	ٚB	�B	�B	�SB	�YB	��B	�B	ںB	��B	�B	�B	��B	� B	��B	��B	��B	�B	�HB	��B	�hB	�B	�B	��B	�;B	޸B	�5B	ݲB	��B	�B	�bB	��B	�ZB	��B	�cB	�B	�B	�LB	�QB	�B	��B	�OB	�wB	�iB	��B	�B	��B	�aB	�TB	��B	��B	��B	��B	��B	��B	�B	�oB	�B	�B	��B	�B	�qB	�B	�QB	�B	�KB	�B	�yB	�B	�B	�B	��B	�/B	�B	�B	�B	�aB	�-B	�B	�;B	�!B	��B	��B	��B	�hB	�B	��B	��B	�nB	��B	�TB	�B	��B	�B	�?B	�tB	�tB	��B	��B	��B	��B	��B	�+B	�%B	��B	�B	��B	�	B	�*B	�DB	��B	��B	�B	�PB	�dB	�>B	�RB	��B	��B	�RB	��B	��B	�B	��B	�0B	�$B	��B	��B	��B	��B	��B	�B	��B	��B	�B	�B	�(B	��B	��B	��B	��B	��B	�6B	�(B	��B	�B	��B	�cB	��B	��B	��B
 B
 4B	��B
 B	��B	��B	��B	�HB	�B	�HB	�B	�HB	��B	��B	�wB	�]B	�wB	�wB	�BB	�]B	�(B	�(B	�BB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 4B
 B
 B
 �B
 �B
 �B
UB
�B
�B
�B
�B
�B
GB
aB
B
-B
�B
�B
aB
�B
�B
�B
�B
�B
�B
gB
B
�B
9B
�B
�B
MB
�B
�B
-B
B
�B
�B
 B
�B
�B
oB
 �B
 OB
  B
 B	��B	��B	��B	��B	��B	��B	�B	��B	��B	�}B	�}B	��B
  B
 B
 �B
 �B
 B
oB
[B
�B
�B
{B
B
�B
�B
�B
�B
YB
?B
B
?B
EB
�B
�B
�B
1B
fB
�B
�B
�B
	�B

�B

�B
�B
pB
BB
B
B
�B
bB
B
�B
�B
�B
9B
B
�B
�B
�B
$B
�B
�B
QB
�B
#B
�B
�B
�B
�B
IB
dB
~B
B
�B
B
�B
�B
B
�B
 BB
 \B
 �B
 �B
 �B
 �B
 �B
 �B
 �B
!B
!�B
!�B
"�B
"�B
"�B
"�B
"�B
#:B
#:B
#:B
#:B
#nB
#�B
$B
$ZB
$�B
$�B
$�B
%B
%,B
%B
%FB
%FB
%zB
%`B
%`B
%zB
%zB
%�B
%�B
%�B
%�B
%`B
&LB
&LB
&2B
&�B
&�B
&�B
'�B
'�B
(sB
(�B
)yB
*0B
+kB
+�B
+�B
-wB
-�B
-�B
.}B
/iB
/�B
/�B
/�B
/�B
0UB
0�B
1vB
1�B
2aB
2aB
2|B
2�B
3B
3hB
4TB
4�B
4�B
5%B
5?B
5�B
5�B
5�B
6B
6zB
6�B
6�B
72B
7�B
7�B
7�B
88B
88B
8�B
9$B
9�B
9�B
9�B
:*B
:^B
:xB
:^B
:�B
:�B
;B
;�B
;�B
;�B
<6B
<6B
<�B
<�B
<�B
<�B
="B
="B
=�B
=�B
=�B
>B
>(B
>wB
>�B
>�B
>�B
?B
?cB
?}B
?cB
?}B
?}B
@ B
@ B
@4B
@iB
@�B
@�B
AUB
A�B
A�B
A�B
A�B
A�B
B[B
B�B
B�B
B�B
B�B
B�B
B�B
C�B
D3B
D3B
D�B
D�B
D�B
E�B
E9B
ESB
E�B
FYB
F�B
F�B
F�B
F�B
F�B
GzB
G_B
GzB
G_B
G�B
GzB
HB
HB
H�B
H�B
IlB
I�B
I�B
I�B
J	B
J�B
J�B
KB
KxB
K^B
KDB
J�B
J�B
J�B
J�B
J�B
J#B
I�B
J#B
J#B
I�B
J	B
J#B
JXB
J�B
J�B
J�B
K)B
KDB
KxB
KxB
KxB
K�B
K�B
LB
L0B
L0B
L�B
MPB
M�B
N�B
N�B
N�B
OB
O�B
O�B
O�B
O�B
PbB
P�B
Q4B
QNB
Q�B
Q�B
Q�B
RB
RB
R:B
RTB
RoB
R�B
R�B
S[B
S�B
T,B
TFB
T,B
UB
T�B
T�B
U�B
U�B
VB
V�B
W?B
W�B
W�B
X+B
X_B
X�B
X�B
ZB
ZB
ZB
ZB
Z�B
[	B
[	B
[=B
[�B
[qB
[�B
[�B
[qB
[qB
[�B
[�B
[�B
\)B
\xB
\B
\]B
\)B
\xB
\�B
]B
]�B
]�B
^B
^�B
^�B
_VB
_�B
_�B
_�B
_�B
`B
`B
`BB
`�B
`vB
`�B
aB
a-B
a�B
a�B
a�B
a�B
a�B
a�B
bNB
b�B
b�B
c�B
c�B
c�B
d�B
d�B
d�B
e,B
e`B
ezB
e�B
e�B
fB
f�B
gRB
g�B
g�B
g�B
g�B
g�B
h
B
h>B
h�B
i�B
i�B
i�B
jKB
jeB
j�B
j�B
kB
k6B
kB
kkB
k�B
l=B
lqB
l�B
l�B
l�B
l�B
l�B
l�B
l�B
mB
m�B
m�B
m�B
m�B
n/B
n�B
o B
oiB
o�B
o�B
pB
pUB
p�B
p�B
p�B
qB
q'B
q'B
q�B
rB
rGB
raB
r�B
r�B
r�B
sB
s3B
sMB
s�B
s�B
s�B
tB
tnB
tTB
tTB
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
uZB
uZB
utB
utB
u�B
v+B
v+B
v�B
v�B
v�B
v�B
v�B
w2B
wfB
wLB
w�B
w�B
w�B
xB
x8B
xlB
x�B
x�B
y$B
yXB
y$B
y>B
y�B
y�B
z*B
z�B
z�B
z�B
{B
{dB
{�B
{�B
|B
|B
|B
|B
|B
|PB
|�B
|�B
|�B
}B
}<B
}<B
}qB
}�B
}�B
}�B
}�B
~]B
~]B
~�B
~�B
~�B
B
HB
.B
.B
HB
�B
�B
�B
�B
�iB
��B
��B
� B
��B
��B
��B
�B
��B
��B
�B
�AB
�'B
��B
��B
�B
�aB
�{B
�{B
��B
�MB
�MB
�gB
��B
��B
��B
��B
��B
��B
�B
��B
��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604104924  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604174015  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604174016  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604174016                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605024023  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605024023  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610131507                      G�O�G�O�G�O�                