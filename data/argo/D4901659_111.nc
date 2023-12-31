CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2017-08-13T17:02:21Z creation      
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
_FillValue                 �  I    PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  `�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  pX   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �@   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ʌ   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �x   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �(   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �X   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �X   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �X   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �X   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �    HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �        �Argo profile    3.1 1.2 19500101000000  20170813170221  20230721230912  4901659 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               oA   AO  5382                            2C  D   NAVIS_A                         0371                            082713                          863 @�����1   @�Q�z@9�Z�1�c�\(�1   GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                      oA   A   A   @�  @�  A   A   A@  A`  A�  A�  A�  A�  A���A���A���A���B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�33B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� DfD� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DJ��DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D��D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�fD�@ 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@z�H@�p�@�p�A�RA>�RA^�RA~�RA�\)A�\)A�\)A�(�A�(�A�(�A�(�A�\)B�B�B�B�B'�B/�B7�B?�BG�BO�BW�B_�Bg�Bo�Bw�B�
=B���B��
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
B��
B��
B��
B��
C�C�C�C�C	�C�C�C�C�C�C�C�C�C�C�C�C!�C#�C%�C'�C)�C+�C-�C/�C1�C3�C5�C7�C9�C;�C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D z�D ��Dz�D��Dz�D��Dz�D��Dz�D��Dz�DGDz�D��Dz�D��Dz�D��D	z�D	��D
z�D
��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��D z�D ��D!z�D!��D"z�D"��D#z�D#��D$z�D$��D%z�D%��D&z�D&��D'z�D'��D(z�D(��D)z�D)��D*z�D*��D+z�D+��D,z�D,��D-z�D-��D.z�D.��D/z�D/��D0z�D0��D1z�D1��D2z�D2��D3z�D3��D4z�D4��D5z�D5��D6z�D6��D7z�D7��D8z�D8��D9z�D9��D:z�D:��D;z�D;��D<z�D<��D=z�D=��D>z�D>��D?z�D?��D@z�D@��DAz�DA��DBz�DB��DCz�DC��DDz�DD��DEz�DE��DFz�DF��DGz�DG��DHz�DH��DIz�DI��DJz�DJ�{DKz�DK��DLz�DL��DMz�DM��DNz�DN��DOz�DO��DPz�DP��DQz�DQ��DRz�DR��DSz�DS��DTz�DT��DUz�DU��DVz�DV��DWz�DW��DXz�DX��DYz�DY��DZz�DZ��D[z�D[��D\z�D\��D]z�D]��D^z�D^��D_z�D_��D`z�D`��Daz�Da��Dbz�Db��Dcz�Dc��Ddz�Dd��Dez�De��Dfz�Df��Dgz�Dg��Dhz�Dh��Diz�Di��Djz�Dj��Dkz�Dk��Dlz�Dl��Dmz�Dm��Dnz�Dn��Doz�Do��Dpz�Dp��Dqz�Dq��Drz�Dr��Dsz�Ds��Dtz�Dt��Duz�Du��Dvz�Dv��Dwz�Dw��Dxz�Dx��Dyz�Dy��Dzz�Dz��D{z�D{��D|z�D|��D}z�D}��D~z�D~��Dz�D��D�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD� �D�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD½qD��qD�=qD�}qDýqD��qD�=qD�}qDĽqD��qD�=qD�}qDŽqD��qD�=qD�}qDƽqD��qD�=qD�}qDǽqD��qD�=qD�}qDȽqD��qD�=qD�}qDɽqD��qD�=qD�}qDʽqD��qD�=qD�}qD˽qD��qD�=qD�}qD̽qD��qD�=qD�}qDͽqD��qD�=qD�}qDνqD��qD�=qD�}qDϽqD��qD�=qD�}qDнqD��qD�=qD�}qDѽqD��qD�=qD�}qDҽqD��qD�=qD�}qDӽqD��qD�=qD�}qDԽqD��qD�=qD�}qDսqD��qD�=qD�}qDֽqD��qD�=qD�}qD׽qD��qD�=qD�}qDؽqD��qD�=qD�}qDٽqD��qD�=qD�}qDڽqD��qD�=qD�}qD۽qD��qD�=qD�}qDܽqD��qD�=qD�}qDݽqD��qD�=qD�}qD޽qD��qD�=qD�}qD߽qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�>D��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD��qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��D�=q11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�jA�p�A�r�A�r�A�r�A�r�A�r�A�p�A�p�A�p�A�r�A�t�A�p�A�n�A�n�A�v�A�n�A�p�A�v�A۩�A� �A��A�Q�A���A؅Aכ�AՅA�bA�5?Aɇ+AǾwA�^5A��A� �A�?}A�Q�A�?}A�  A��DA�VA��HA���A��+A���A���A��A��`A�oA��-A���A�-A�hsA��9A���A�dZA�~�A���A��A�x�A���A��A�oA�ȴA��A�XA��HA��jA�v�A���A���A�A��yA�&�A�K�A��A��A�hsA��HA�A�A��A���A�7LA��HA���A��PA�\)A���A��jA�9XA���A��A���A��PA��A��/A���A��A�FA}+Az�DAxI�AvbAt$�AsdZArM�Ap�jAo`BAm��Al�`AlAj�uAhVAf�DAc�Aa�A_`BA^�+A^  A]�A]��A\9XAW�AT�!AT�+ATn�AS��AP��ANJAM|�AMAMdZAL�jAL�AK��AK&�AJ~�AJbAI�AH�\AH�AG��AG|�AG%AFr�AF=qAE/AC��AB5?AAx�A@��A@{A?�FA>��A>�A=��A=�A=S�A<�yA<�uA<ZA;��A9��A9/A933A9�A8r�A7�^A6�HA6�DA6A5t�A4A�A2��A2r�A2A1t�A1G�A17LA0��A0�A0�A/+A.�uA.bA,�A+�A)x�A(r�A'A'"�A&�RA&I�A%7LA#�mA#S�A"VA!7LA ��A�A?}AĜA9XA33A�9AffA��A(�A�FAt�AbA�/A(�AJA  A�A�A�wA�A�^A�yA1'A��A�Av�A5?A�^A�yAM�AƨA
VA	l�An�AJA�#A?}A��A�^AAbNA��A%A=qAbA�A+A �`A 9X@�o@�-@�?}@�E�@�V@��@���@�Q�@��
@�l�@�M�@�j@��D@�r�@�  @�@��/@�j@�1@�+@��@��/@�hs@���@◍@��@�?}@�M�@�hs@ۥ�@ٙ�@�33@�v�@ա�@�X@�bN@���@ЋD@�1@���@�"�@�/@�b@�K�@�@��@�1'@�E�@őh@Ų-@�hs@ēu@�33@§�@���@�I�@���@�@�~�@��T@��7@��@�t�@���@�@��`@�t�@�33@��R@�=q@��T@�Ĝ@��@���@�1'@�S�@��@�?}@���@��j@�9X@�K�@���@��9@���@�l�@�S�@��+@��@�Z@�"�@�v�@���@�p�@�X@��u@�  @��F@��@�+@�^5@��-@�p�@���@��@�Q�@�(�@��m@��@�b@���@��w@�1'@�Z@�A�@��@��
@��R@�V@�{@��@��^@��7@��@�r�@���@�\)@��!@�E�@�J@�@���@��@���@��@� �@�1@��
@�K�@�dZ@�S�@��P@���@�K�@�ȴ@�V@���@�7L@���@�Q�@�1@��@��m@��F@�@���@�V@�5?@�G�@�j@���@�ƨ@�dZ@��!@�5?@���@��-@�G�@�Ĝ@�Z@��@�j@�I�@�Q�@�9X@��
@�\)@�@�ȴ@�$�@�@��@�O�@���@���@�Z@�b@�\)@��@�ȴ@��!@���@�J@�`B@�&�@���@���@�bN@�(�@��@��;@�l�@��y@���@�n�@�v�@��+@�V@�J@��@�@���@���@���@��@��@�X@�O�@�7L@�%@���@�V@�%@���@��`@���@�Ĝ@��9@���@��D@�bN@�Z@�A�@�A�@�9X@�1'@�b@;d@~E�@}�h@}`B@|��@|(�@{�F@{"�@zJ@yx�@y%@xb@vȴ@u�T@t��@s33@r�!@r~�@q��@q��@q�7@q&�@p�`@pQ�@p �@o�;@o�@oK�@n��@n�y@nȴ@n�R@nV@m�h@mO�@l�j@l�@l�D@k��@j��@jn�@j^5@jM�@j-@j�@i��@iX@i�@hĜ@h��@h �@g�@fȴ@e`B@d�/@d�@cƨ@cC�@b�@bJ@a�^@a�7@`r�@_�@_\)@_;d@_+@^��@^�@^�R@^@]@]`B@]�@\�@\��@\z�@\Z@\�@[�m@[�m@[t�@Z�@Z��@Z�!@Z~�@Zn�@Z=q@Y��@Y7L@X��@X �@WK�@V�@Vȴ@V�R@V��@V��@VE�@VE�@V5?@U�T@U��@UO�@U/@U�@UV@T�@T�/@T��@TI�@T(�@T(�@T9X@Tj@TI�@T9X@T1@S�@S��@S33@S@RJ@Q�#@Q�#@Q�#@Q�#@Q�#@Qhs@P��@P �@O�@O��@O��@O\)@N��@N��@Nv�@N{@M@M�@Mp�@Mp�@M`B@M�@MV@L��@L�j@LZ@L(�@L1@K�
@K�@KdZ@K33@Ko@J��@J�\@JM�@J-@JJ@I�@I�^@I�7@I7L@I7L@I&�@H��@HA�@G��@G�@G��@G\)@G\)@G\)@Gl�@Gl�@G\)@GK�@G�@F�y@F�R@Fff@FV@F5?@F{@E�@D�@D�@D(�@C�
@C�
@Cƨ@CS�@C33@C"�@C@B��@BM�@A�^@AX@AG�@A%@@�`@A%@@��@@��@@r�@?��@>�@>��@>E�@=/@<��@<��@<�D@<��@<��@<�@<�@<Z@<1@;�F@;��@;dZ@:�@:^5@9�@9��@9%@8�u@8b@7�;@7��@7�P@7�P@7|�@7\)@6��@5��@5?}@5/@4��@4�j@4��@4j@3��@3��@3dZ@3@2�\@2=q@1��@1�7@1�@0��@0A�@/��@/�w@/��@/��@/�@/�@/�@/��@/|�@/;d@.��@-@-p�@-�@,��@,��@,j@+��@+�m@+�
@+ƨ@+��@+t�@+o@*�H@*��@*~�@)�@)��@)�7@)hs@)G�@(��@(�9@(r�@(Q�@(b@'�@'�w@'K�@';d@&�@&ȴ@&V@&$�@%��@%V@$�@$��@$Z@$9X@$(�@#�
@#S�@#o@"~�@!��@!��@!�7@!X@!�@ Ĝ@ r�@ A�@��@��@�P@l�@�@�R@��@��@��@v�@ff@V@$�@@O�@/@/@/@/@/@�@�/@��@I�@�F@��@t�@S�@S�@33@@�H@��@n�@J@�^@x�@X@&�@��@�u@Q�@�@��@|�@K�@+@��@�R@��@v�@@�-@�h@`B@?}@�@�/@�@Z@1@�
@�@"�@��@n�@M�@-@J@��@�#@��@��@hs@&�@��@Ĝ@�9@��@r�@A�@�@��@��@�@|�@;d@�@
=@��@�@��@��@v�@V@$�@@��@�-@�h@�@`B@O�@?}@��@�j@�@��@z�@Z@9X@(�@1@��@�m@�F@S�@33@"�@o@@
�H@
��@
��@
~�@
n�@
=q@
�@	�@	�^@	x�@	7L@�`@Ĝ@�9@�u@Q�@Q�@Q�@b@�;@�;@�;@�;@�w@�P@�P@|�@+@��@V@$�@�@?}@�@�/@�j@�@��@��@�D@�D@�D@j@ƨ@t�@dZ@C�@o@�H@��@n�@M�@=q@-@�@�@J@��@�@��@�^@�^@�^@�^@�^@�^@�7@7L@ �`@ Ĝ@ Ĝ@ Ĝ@ Ĝ@ Ĝ@ �@ bN@ bN@ Q�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�jA�p�A�r�A�r�A�r�A�r�A�r�A�p�A�p�A�p�A�r�A�t�A�p�A�n�A�n�A�v�A�n�A�p�A�v�A۩�A� �A��A�Q�A���A؅Aכ�AՅA�bA�5?Aɇ+AǾwA�^5A��A� �A�?}A�Q�A�?}A�  A��DA�VA��HA���A��+A���A���A��A��`A�oA��-A���A�-A�hsA��9A���A�dZA�~�A���A��A�x�A���A��A�oA�ȴA��A�XA��HA��jA�v�A���A���A�A��yA�&�A�K�A��A��A�hsA��HA�A�A��A���A�7LA��HA���A��PA�\)A���A��jA�9XA���A��A���A��PA��A��/A���A��A�FA}+Az�DAxI�AvbAt$�AsdZArM�Ap�jAo`BAm��Al�`AlAj�uAhVAf�DAc�Aa�A_`BA^�+A^  A]�A]��A\9XAW�AT�!AT�+ATn�AS��AP��ANJAM|�AMAMdZAL�jAL�AK��AK&�AJ~�AJbAI�AH�\AH�AG��AG|�AG%AFr�AF=qAE/AC��AB5?AAx�A@��A@{A?�FA>��A>�A=��A=�A=S�A<�yA<�uA<ZA;��A9��A9/A933A9�A8r�A7�^A6�HA6�DA6A5t�A4A�A2��A2r�A2A1t�A1G�A17LA0��A0�A0�A/+A.�uA.bA,�A+�A)x�A(r�A'A'"�A&�RA&I�A%7LA#�mA#S�A"VA!7LA ��A�A?}AĜA9XA33A�9AffA��A(�A�FAt�AbA�/A(�AJA  A�A�A�wA�A�^A�yA1'A��A�Av�A5?A�^A�yAM�AƨA
VA	l�An�AJA�#A?}A��A�^AAbNA��A%A=qAbA�A+A �`A 9X@�o@�-@�?}@�E�@�V@��@���@�Q�@��
@�l�@�M�@�j@��D@�r�@�  @�@��/@�j@�1@�+@��@��/@�hs@���@◍@��@�?}@�M�@�hs@ۥ�@ٙ�@�33@�v�@ա�@�X@�bN@���@ЋD@�1@���@�"�@�/@�b@�K�@�@��@�1'@�E�@őh@Ų-@�hs@ēu@�33@§�@���@�I�@���@�@�~�@��T@��7@��@�t�@���@�@��`@�t�@�33@��R@�=q@��T@�Ĝ@��@���@�1'@�S�@��@�?}@���@��j@�9X@�K�@���@��9@���@�l�@�S�@��+@��@�Z@�"�@�v�@���@�p�@�X@��u@�  @��F@��@�+@�^5@��-@�p�@���@��@�Q�@�(�@��m@��@�b@���@��w@�1'@�Z@�A�@��@��
@��R@�V@�{@��@��^@��7@��@�r�@���@�\)@��!@�E�@�J@�@���@��@���@��@� �@�1@��
@�K�@�dZ@�S�@��P@���@�K�@�ȴ@�V@���@�7L@���@�Q�@�1@��@��m@��F@�@���@�V@�5?@�G�@�j@���@�ƨ@�dZ@��!@�5?@���@��-@�G�@�Ĝ@�Z@��@�j@�I�@�Q�@�9X@��
@�\)@�@�ȴ@�$�@�@��@�O�@���@���@�Z@�b@�\)@��@�ȴ@��!@���@�J@�`B@�&�@���@���@�bN@�(�@��@��;@�l�@��y@���@�n�@�v�@��+@�V@�J@��@�@���@���@���@��@��@�X@�O�@�7L@�%@���@�V@�%@���@��`@���@�Ĝ@��9@���@��D@�bN@�Z@�A�@�A�@�9X@�1'@�b@;d@~E�@}�h@}`B@|��@|(�@{�F@{"�@zJ@yx�@y%@xb@vȴ@u�T@t��@s33@r�!@r~�@q��@q��@q�7@q&�@p�`@pQ�@p �@o�;@o�@oK�@n��@n�y@nȴ@n�R@nV@m�h@mO�@l�j@l�@l�D@k��@j��@jn�@j^5@jM�@j-@j�@i��@iX@i�@hĜ@h��@h �@g�@fȴ@e`B@d�/@d�@cƨ@cC�@b�@bJ@a�^@a�7@`r�@_�@_\)@_;d@_+@^��@^�@^�R@^@]@]`B@]�@\�@\��@\z�@\Z@\�@[�m@[�m@[t�@Z�@Z��@Z�!@Z~�@Zn�@Z=q@Y��@Y7L@X��@X �@WK�@V�@Vȴ@V�R@V��@V��@VE�@VE�@V5?@U�T@U��@UO�@U/@U�@UV@T�@T�/@T��@TI�@T(�@T(�@T9X@Tj@TI�@T9X@T1@S�@S��@S33@S@RJ@Q�#@Q�#@Q�#@Q�#@Q�#@Qhs@P��@P �@O�@O��@O��@O\)@N��@N��@Nv�@N{@M@M�@Mp�@Mp�@M`B@M�@MV@L��@L�j@LZ@L(�@L1@K�
@K�@KdZ@K33@Ko@J��@J�\@JM�@J-@JJ@I�@I�^@I�7@I7L@I7L@I&�@H��@HA�@G��@G�@G��@G\)@G\)@G\)@Gl�@Gl�@G\)@GK�@G�@F�y@F�R@Fff@FV@F5?@F{@E�@D�@D�@D(�@C�
@C�
@Cƨ@CS�@C33@C"�@C@B��@BM�@A�^@AX@AG�@A%@@�`@A%@@��@@��@@r�@?��@>�@>��@>E�@=/@<��@<��@<�D@<��@<��@<�@<�@<Z@<1@;�F@;��@;dZ@:�@:^5@9�@9��@9%@8�u@8b@7�;@7��@7�P@7�P@7|�@7\)@6��@5��@5?}@5/@4��@4�j@4��@4j@3��@3��@3dZ@3@2�\@2=q@1��@1�7@1�@0��@0A�@/��@/�w@/��@/��@/�@/�@/�@/��@/|�@/;d@.��@-@-p�@-�@,��@,��@,j@+��@+�m@+�
@+ƨ@+��@+t�@+o@*�H@*��@*~�@)�@)��@)�7@)hs@)G�@(��@(�9@(r�@(Q�@(b@'�@'�w@'K�@';d@&�@&ȴ@&V@&$�@%��@%V@$�@$��@$Z@$9X@$(�@#�
@#S�@#o@"~�@!��@!��@!�7@!X@!�@ Ĝ@ r�@ A�@��@��@�P@l�@�@�R@��@��@��@v�@ff@V@$�@@O�@/@/@/@/@/@�@�/@��@I�@�F@��@t�@S�@S�@33@@�H@��@n�@J@�^@x�@X@&�@��@�u@Q�@�@��@|�@K�@+@��@�R@��@v�@@�-@�h@`B@?}@�@�/@�@Z@1@�
@�@"�@��@n�@M�@-@J@��@�#@��@��@hs@&�@��@Ĝ@�9@��@r�@A�@�@��@��@�@|�@;d@�@
=@��@�@��@��@v�@V@$�@@��@�-@�h@�@`B@O�@?}@��@�j@�@��@z�@Z@9X@(�@1@��@�m@�F@S�@33@"�@o@@
�H@
��@
��@
~�@
n�@
=q@
�@	�@	�^@	x�@	7L@�`@Ĝ@�9@�u@Q�@Q�@Q�@b@�;@�;@�;@�;@�w@�P@�P@|�@+@��@V@$�@�@?}@�@�/@�j@�@��@��@�D@�D@�D@j@ƨ@t�@dZ@C�@o@�H@��@n�@M�@=q@-@�@�@J@��@�@��@�^@�^@�^@�^@�^@�^@�7@7L@ �`@ Ĝ@ Ĝ@ Ĝ@ Ĝ@ Ĝ@ �@ bN@ bN@ Q�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�mB�fB�fB�fB�mB�mB�fB�mB�mB�mB�mB�mB�mB�mB�mB�sB�mB�mB�fB��B�XB�^B��BBĜBȴB��B�B�;B�5B�B��B��B��B��BƨB��B�XB�B�JBx�Bv�BbNB^5BaHBe`B`BBW
BM�B>wB/B&�B"�B�B
=B  B��B�B�B�yB�mB�TB�B��BĜB��B�wB�dB�3B��B�hB�By�Bn�BcTB\)BS�BK�BB�B=qB8RB49B/B �B�B�BbBJBB
��B
�yB
��B
ŢB
�wB
�dB
�LB
��B
�\B
y�B
bNB
M�B
?}B
0!B
(�B
�B
uB
%B	��B	�B	�yB	�)B	��B	�wB	��B	��B	z�B	p�B	gmB	ffB	`BB	I�B	�B	%B	B	B	  B��B�B��B	hB	�B	uB	�B	)�B	&�B	#�B	!�B	�B	�B	�B	uB	hB	VB	JB	
=B	B��B��B�B�B�B�B�fB�ZB�NB�HB�HB�;B�5B�)B�B�B�
B�B�
B�5B�BB�/B�/B�#B�B��B��B��B�B�B�B��B��B��B��B��BɺBƨB��B�jB�9B�'B�B�B��B��B��B��B��B��B��B�oB�\B�JB�7B�+B�%B�B�B� B{�Bz�Bx�Bt�Bq�Bp�Bp�Bo�Bn�BjBgmBdZBbNB`BB^5B\)BZBYBXBVBS�BP�BN�BJ�BG�BE�BD�BC�B@�B>wB>wB=qB<jB:^B8RB7LB7LB49B33B2-B1'B1'B0!B-B)�B(�B(�B'�B&�B%�B$�B#�B#�B#�B"�B!�B!�B!�B �B �B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B �B#�B'�B,B+B+B0!B49B49B49B6FB49B0!B33B5?B6FB6FB8RB9XB;dB>wB?}B@�BB�BE�BF�BG�BH�BI�BL�BL�BK�BK�BL�BN�BO�BP�BQ�BQ�BS�BS�BXBZB[#BZB[#B`BBcTBe`Be`Bk�Bn�Bn�Bp�Br�Br�Br�Br�Bt�Bv�B~�B�1B�JB�PB�bB�{B��B��B��B��B�B�'B�-B�3B�3B�9B�FB�dB�jB�qB�qB�}BŢBȴB��B��B��B��B��B��B��B�B�5B�;B�BB�BB�HB�ZB�fB�B�B�B�B�B�B��B��B��B��B��B	B	B	B	%B	+B	%B	%B	+B	DB	VB	\B	bB	bB	hB	oB	uB	�B	�B	 �B	!�B	"�B	#�B	$�B	&�B	&�B	&�B	)�B	,B	-B	-B	.B	1'B	2-B	33B	5?B	:^B	<jB	<jB	?}B	C�B	E�B	F�B	F�B	G�B	H�B	I�B	K�B	L�B	L�B	L�B	M�B	N�B	P�B	R�B	S�B	T�B	T�B	T�B	VB	W
B	YB	[#B	[#B	]/B	^5B	^5B	_;B	_;B	aHB	cTB	e`B	hsB	jB	k�B	k�B	k�B	l�B	n�B	p�B	q�B	s�B	t�B	u�B	w�B	y�B	z�B	|�B	}�B	� B	�B	�B	�B	�+B	�1B	�7B	�DB	�JB	�VB	�VB	�VB	�hB	�uB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�!B	�'B	�'B	�-B	�-B	�-B	�9B	�?B	�?B	�FB	�LB	�RB	�^B	�qB	�}B	B	ĜB	ŢB	ƨB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�)B	�/B	�/B	�5B	�;B	�BB	�BB	�NB	�ZB	�`B	�`B	�fB	�fB	�mB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
%B
+B
1B
	7B
	7B

=B

=B

=B

=B
DB
DB
DB
DB
PB
PB
VB
VB
\B
\B
bB
bB
hB
hB
hB
oB
uB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
!�B
!�B
!�B
!�B
#�B
$�B
$�B
$�B
$�B
#�B
#�B
#�B
#�B
#�B
%�B
&�B
'�B
'�B
'�B
(�B
+B
+B
+B
,B
-B
/B
/B
/B
0!B
0!B
0!B
1'B
2-B
2-B
2-B
2-B
33B
2-B
2-B
2-B
1'B
1'B
2-B
33B
5?B
5?B
5?B
5?B
5?B
7LB
8RB
8RB
8RB
8RB
8RB
8RB
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
=qB
>wB
>wB
>wB
>wB
>wB
>wB
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
D�B
D�B
E�B
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
H�B
H�B
H�B
I�B
I�B
I�B
I�B
J�B
K�B
K�B
K�B
K�B
L�B
L�B
L�B
M�B
M�B
N�B
N�B
O�B
O�B
O�B
P�B
P�B
P�B
Q�B
Q�B
R�B
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
S�B
T�B
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
\)B
\)B
\)B
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
`BB
`BB
`BB
`BB
`BB
aHB
aHB
aHB
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
e`B
e`B
e`B
e`B
e`B
ffB
ffB
ffB
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
p�B
q�B
q�B
p�B
q�B
q�B
q�B
q�B
q�B
r�B
r�B
r�B
s�B
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
v�B
v�B
w�B
w�B
w�B
w�B
w�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
z�B
z�B
z�B
z�B
z�B
z�B
{�B
{�B
{�B
{�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B�WB�dB�nB�lB�wB�tB�sB�wB�tB�nB�kB�B�~B�tB�aB�B�nB�nB�B׺B�+B��B�HBÓBƌB�B�FB�bB�$B�B��B�&B�+BոBՄB��B�RB�QB��B��By�B~�Bd�BbzBe@Bj}Bc�BX�BR�BE�B2�B*�B'�BsBB	�B�&B�B�B�$B�B��B�;B�rB�RB�B��B��B��B��B��B�%B}kBq�Be�B^�BVcBN�BDB>�B:+B5�B3SB"�B�B5B�B�BIB
��B
�B
ϩB
ǌB
�uB
�TB
��B
�.B
��B
�B
gRB
R�B
D	B
1�B
+�B
#;B
�B

9B	�XB	�B	��B	�\B	��B	�AB	��B	��B	|�B	q�B	g�B	gB	c�B	T@B	!B	�B	tB	kB	IB��B�B�_B	^B	#B	OB	�B	,EB	(�B	%B	#B	!�B	�B	lB	uB	�B	�B	"B	iB	�B��B�B��B�B��B�3B�B��B��B��B�vB�:B��BއB�dB�KB�B؁B��B�[B�B�+BލBܓB��BԔB��B��B�HB�rB�7BՙB� B��B��B�*B��BɇB��B�hB��B��B��B�1B�>B��B�B�HB��B��B��B��B�DB��B��B��B�rB��B�/B��B}B{�B|QBw�Bs�Bp�Bp�Bo�Bp�Bm�Bj?Bf�Bd_BbB_�B]�B[NBY�BYfBX\BU�BR�BR�BMPBJBF�BEMBEeBB�BAB@�B?cB>YB<�B:�B8 B9B5dB45B4cB3_B2�B1�B2B0mB*�B)�B(�B'�B&�B&�B&IB$1B$B#�B%�B"�B"�B!B"DB!�B �B!�B 1BuB�B$B�B�B�BBQB�B�B B�B �B B dB B�B�B B!�B%�B)B-B-TB+�B0B4�B5`B6B7B6\B1
B41B6	B7B7B8�B:�B<�B?-B@�BB/BD�BFBGfBHXBI0BJ�BL�BL�BLBBL�BN(BO�BPhBQBR�BS BT�BVABX�BZ�B[LB[$B\�Ba6Bd�BfHBfBk�Bn�Bo�Bq]BsBr�Bs/Bs�Bu�Bw0B�B�hB��B��B��B��B�B�B��B�KB��B�UB�lB��B��B��B��B��B��B��B�B�lB�XBɘB��B�iB�"B�>B��B�tB�}BُB��B�iB��B� B�4B�yB�#B�mB�B�NB�CB�nB��B��B�PB�PB��B�LB	bB	B	�B	�B	qB	�B	nB	�B	�B	�B	xB	B	�B	�B	B	HB	/B	}B	 �B	"B	"�B	$	B	%vB	'�B	'wB	'NB	*�B	,�B	-^B	-OB	.�B	1qB	2�B	3�B	6B	:�B	<�B	<�B	?�B	DPB	F�B	F�B	GB	G�B	I
B	JB	K�B	M$B	MkB	M}B	N4B	O1B	P�B	R�B	TCB	UkB	U+B	ULB	VGB	WB	Y0B	[QB	[0B	]uB	^JB	^eB	_�B	__B	a/B	cmB	e�B	h�B	j�B	k�B	k�B	k�B	l�B	n�B	p�B	q�B	s�B	t�B	u�B	xB	z�B	{�B	}�B	~(B	�B	��B	��B	��B	�B	��B	��B	�B	�QB	�B	�LB	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	�B	�$B	��B	�B	��B	�;B	��B	�5B	�qB	� B	�/B	��B	��B	�tB	�9B	�=B	�HB	�FB	�VB	��B	�wB	��B	�fB	��B	��B	�B	�|B	��B	�B	��B	�B	��B	�[B	�
B	��B	̦B	�uB	�)B	�B	�B	�B	�B	�"B	ՐB	�CB	�bB	�MB	�sB	�@B	�LB	�NB	�cB	�gB	�OB	��B	�B	�B	�uB	�B	�}B	�B	��B	��B	��B	�B	� B	��B	�B	�B	�B	�B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�3B	��B	��B	��B	��B	��B	��B	�B	�DB	��B	�2B	�B	��B	�"B	�B	�B	�	B	�
B
 kB
�B
|B
>B
1B
AB
SB
sB
|B
TB
�B
	�B
	wB

SB

IB

[B

~B
]B
^B
�B
�B
�B
xB
�B
�B
B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
	B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
8B
6B
 B
!@B
"B
!�B
!�B
"2B
#�B
$�B
%B
%B
%\B
$YB
$.B
#�B
$B
#�B
%�B
'B
(#B
(*B
(�B
)�B
+FB
+\B
+�B
,�B
-B
/0B
/B
/%B
0B
0*B
0bB
1gB
2iB
2CB
2fB
2�B
3�B
2�B
2PB
2�B
1{B
1�B
2VB
3fB
5YB
5IB
5SB
5dB
5�B
8B
8�B
8gB
8{B
8�B
8rB
8B
9�B
:�B
:�B
;�B
;�B
;�B
<�B
<�B
<�B
<�B
=�B
>�B
>�B
>�B
>�B
>vB
>B
?�B
?�B
?�B
?�B
?�B
A0B
A�B
A�B
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
D�B
EB
E�B
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
IB
H�B
IB
I�B
JB
I�B
J/B
K6B
K�B
LB
LB
K�B
L�B
MB
M2B
NB
N:B
O0B
N�B
PB
PB
PB
Q!B
Q"B
QB
R?B
RB
S	B
SB
S0B
S;B
TB
TB
TB
TB
TB
TB
T%B
TBB
UOB
V$B
VB
VB
VB
VB
VB
V<B
V=B
WNB
WyB
X2B
X0B
X3B
XB
X0B
YDB
Y:B
Y:B
Y^B
YjB
ZcB
ZWB
Z@B
[TB
[hB
[\B
\eB
\~B
\nB
]SB
]]B
]TB
]^B
^nB
^LB
^gB
^�B
_�B
_^B
_iB
`fB
`dB
`~B
`tB
`�B
a�B
axB
a�B
b�B
b�B
c�B
cyB
cyB
d|B
dqB
d|B
dqB
d�B
d�B
d�B
e�B
euB
euB
eyB
e�B
f�B
f�B
f�B
gzB
g�B
g�B
g�B
g�B
g�B
h�B
h�B
h�B
h�B
h�B
h�B
h�B
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
k�B
k�B
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
p�B
q�B
q�B
p�B
q�B
q�B
q�B
rB
r=B
r�B
r�B
s:B
s�B
uB
t�B
t�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
vDB
wB
v�B
w�B
w�B
w�B
x	B
w�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
y�B
y�B
y�B
y�B
y�B
y�B
z
B
z#B
z!B
z�B
z�B
z�B
z�B
z�B
{B
|B
{�B
{�B
{�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<�1�<$lP<#�
<#�
<6��<#�
<$�<0	<6��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
</B�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<;>�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - (REPORTED_SURFACE_PRESSURE) for PADJ                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            PSAL_ADJ corrects Cnd. Thermal Mass (CTM), Johnson et al,2007,JAOT & effects of pressure adjustments                                                                                                                                                            PADJ REPORTED_SURFACE_PRESSURE =0.08 dbar                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            CTM alpha = 0.141 & tau = 6.68 s with error equal to the correction                                                                                                                                                                                             Pressures adjusted using reported surface pressure when warranted.                                                                                                                                                                                              none                                                                                                                                                                                                                                                            Salinity corrected for pressure sensor calibration drift and cell thermal mass                                                                                                                                                                                  201810310930042018103109300420181031093004  AO  ARCAADJP                                                                    20170813170221    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20170813170221  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20170813170221  QCF$                G�O�G�O�G�O�0               PM  ARSQPADJV1.1                                                                20181031093004  QC  PRES            @�  D�@ G�O�                PM  ARSQCTM V1.1                                                                20181031093004  QC  PSAL            @�  D�@ G�O�                PM  ARSQCOWGV1.1CTD_2021v2 + Argo_2021v03                                       20230721230912  IP                  G�O�G�O�G�O�                