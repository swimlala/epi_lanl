CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-05-14T00:36:11Z creation;2018-05-14T00:36:16Z conversion to V3.1;2019-12-19T07:38:22Z update;     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7T   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     88   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8X   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8\   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8d   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8h   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8p   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8x   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I@   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M,   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p`   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tL   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �l   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �D   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �x   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �x   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �x   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �x   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �    HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �H   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �X   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �\   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �l   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �p   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �t   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �xArgo profile    3.1 1.2 19500101000000  20180514003611  20200116221515  5905048                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0577_240                     2C  D   NAVIS_A                         0577                            ARGO 102115                     863 @�bu����1   @�bvq��@4���Z���dL����1   GPS     A   B   B   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�ff@�  A   A!��AA��A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"�C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/y�D0  D0� D0��D1y�D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dl��Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�C3DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D��3D�3D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�FfD�c311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��
@�p�@�p�A Q�A@Q�A^�RA~�RA�\)A�\)A�\)A�\)A�\)A�\)A�\)A�\)B�B�B�B�B'�B/�B7�B?�BG�BO�BW�B_�Bg�Bo�Bw�B�B��
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
C�C�C�C�C	�C�C�C�C�C�C�C�C�C�C�C�C"C#�C%�C'�C)�C+�C-�C/�C1�C3�C5�C7�C9�C;�C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D z�D ��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��D�HD��Dz�D��Dz�D��D	z�D	��D
z�D
��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��D z�D ��D!z�D!��D"z�D"��D#z�D#��D$z�D$��D%z�D%��D&z�D&��D'z�D'��D(z�D(��D)z�D)��D*z�D*��D+z�D+��D,z�D,��D-z�D-��D.z�D.��D/t{D/��D0z�D0�{D1t{D1��D2z�D2��D3z�D3��D4z�D4��D5z�D5��D6z�D6��D7z�D7��D8z�D8��D9z�D9��D:z�D:��D;z�D;��D<z�D<��D=z�D=��D>z�D>��D?z�D?��D@z�D@��DAz�DA��DBz�DB��DCz�DC��DDz�DD��DEz�DE��DFz�DF��DGz�DG��DHz�DH��DIz�DI��DJz�DJ��DKz�DK��DLz�DL��DMz�DM��DNz�DN��DOz�DO��DPz�DP��DQz�DQ��DRz�DR��DSz�DS��DTz�DT��DUz�DU��DVz�DV��DWz�DW��DXz�DX��DYz�DY��DZz�DZ��D[z�D[��D\z�D\��D]z�D]��D^z�D^��D_z�D_��D`z�D`��Daz�Da��Dbz�Db��Dcz�Dc��Ddz�Dd��Dez�De��Dfz�Df��Dgz�Dg��Dhz�Dh��Diz�Di��Djz�Dj��Dkz�Dk��Dlz�Dl�{Dmz�Dm��Dnz�Dn��Doz�Do��Dpz�Dp��Dqz�Dq��Drz�Dr��Dsz�Ds��Dtz�Dt��Duz�Du��Dvz�Dv��Dwz�Dw��Dxz�Dx��Dyz�Dy��Dzz�Dz��D{z�D{��D|z�D|��D}z�D}��D~z�D~��Dz�D��D�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD� �D�=qD�}qD½qD��qD�=qD�}qDýqD��qD�=qD�}qDĽqD��qD�=qD�}qDŽqD��qD�=qD�}qDƽqD��qD�=qD�}qDǽqD��qD�=qD�}qDȽqD��qD�=qD�}qDɽqD��qD�=qD�}qDʽqD��qD�=qD�}qD˽qD��qD�=qD�}qD̽qD��qD�=qD�}qDͽqD��qD�=qD�}qDνqD��qD�=qD�}qDϽqD��qD�=qD�}qDнqD��qD�=qD�}qDѽqD��qD�=qD�}qDҽqD��qD�@�D�}qDӽqD��qD�=qD�}qDԽqD��qD�=qD�}qDսqD��qD�=qD�}qDֽqD��qD�=qD�}qD׽qD��qD�=qD�}qDؽqD��qD�=qD�}qDٽqD��qD�=qD�}qDڽqD��qD�=qD�}qD۽qD��qD�=qD�}qDܽqD��qD�=qD�}qDݽqD��qD�=qD�}qD޽qD��qD�=qD�}qD߽qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD��qD��qD�=qD�}qD�qD��qD�=qD�}qD���D� �D�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�C�D�`�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�z�A�~�AʁA�z�AʁAʃAʅAʇ+Aʇ+AʃA�v�A�t�A�t�A�r�A�hsA�ffA�ffA�ffA�`BA�VA�-A�ƨAȕ�A�ZA�v�AŲ-A�{A�G�A�`BA�l�A�=qA���A�Q�A�7LA�^5A���A�(�A�ffA��TA�v�A�~�A�
=A� �A�v�A�VA�?}A�A��^A���A��7A�Q�A���A��-A��A�v�A�?}A��A�oA��TA��A�{A�ĜA�-A���A�A�A��A�jA��9A���A�n�A�33A���A�|�A���A���A� �A��A���A��#A�$�A��A�"�A�t�A�p�A�C�A�Q�A���A�ffA�bA���A�{A���A��A��FA��DA�hsA�I�A�1A��-A�1'A���A�5?A�^5A�XA��FA�~�A��A��A�n�A��hA�jA���A���A�n�A��HA�&�A���A��`A���A�1AO�A~ZA}|�A|��A{�Ay�
Aw�wAsdZAq��Ao��Al�HAi�#Ad�\Ab�A`VA^�yA]��A\��A[�FAZ�uAX�+AWl�AV�9ARz�AP�uAL�AIx�AH�AG��AGXAGC�AF�AD�jAB�AA�A@��A@{A>(�A=�A;�A;VA:bNA9��A9��A9dZA9"�A8�RA8M�A7�7A5�-A4ffA3�
A2��A1?}A/��A/oA.�A.-A-��A,�/A*��A(^5A'/A%�A#�A"��A"E�A!XA �yA ��A ZA�A��AA�7A��An�A��AXAAbAĜA��A�TA�A�HAv�A�^A�A�AoAp�A
VAȴAdZAJA��A1'A�A�;AoA {@�/@���@��T@��9@�?}@��`@�j@�!@�7@��`@��@��@�?}@���@�z�@��@���@�9@�Z@��;@���@�-@��@���@�bN@�9X@�(�@�F@��@���@�Z@�1@���@݉7@�r�@�Q�@���@�33@�&�@���@Չ7@��@��;@�x�@�ƨ@�-@̼j@ˍP@ʟ�@�p�@�%@ȣ�@�j@�(�@š�@�bN@�
=@�@�v�@�E�@��@���@�p�@�?}@���@���@���@���@�@��y@��#@�ff@�v�@�5?@�/@���@�9X@�S�@��\@�x�@��u@� �@�K�@���@��#@���@��@�  @�l�@�33@��!@�=q@�x�@�%@���@���@��
@�C�@�^5@��7@�X@��@��@�j@���@�S�@��y@���@�-@��T@���@�?}@���@�Q�@�1'@�b@��;@��F@�;d@�@���@��\@�^5@��@�X@��`@��j@��D@�j@�Q�@� �@��w@�ƨ@��F@�|�@�
=@��R@��@�@��@�X@�p�@�?}@�Ĝ@�9X@���@��@��y@���@�ff@�-@�@��#@��7@��@���@��/@��D@�1@��F@���@�S�@���@���@��R@���@�^5@�-@�@���@��-@��@���@�z�@�1'@��m@��w@���@�33@�o@�ȴ@��+@�-@���@��^@��7@�p�@�`B@�G�@��@�V@��@��j@���@�Q�@� �@��
@���@�l�@�33@�o@��H@���@���@�v�@�M�@�-@�-@�5?@�-@�-@�J@�@�X@�p�@�x�@�X@�/@��@��9@��@��@��w@�1@�I�@�(�@�(�@�b@���@��H@��R@��\@�J@��h@�7L@�%@���@� �@��m@�|�@�33@�@�ȴ@���@��\@�E�@��T@���@���@�G�@�&�@��/@�Ĝ@�r�@�I�@�I�@��w@�t�@�33@��@�@�@���@�ȴ@�ȴ@��\@�v�@�=q@�=q@�-@�$�@�{@��@��^@��h@�O�@��@�Ĝ@��D@��@�z�@�bN@�Q�@�Q�@�I�@�9X@�1@��@�t�@�S�@�\)@�|�@��@�t�@�K�@�@���@���@���@�~�@�v�@�v�@�V@�=q@�{@��h@�?}@�7L@�&�@�&�@��@���@���@��@�Q�@�(�@��@��@\)@
=@~�@~�+@~@}@}?}@|j@{ƨ@{C�@y��@x��@xQ�@xb@w�@w|�@v��@v�R@v�R@v�+@u�h@uV@t��@tZ@t(�@t�@s�
@so@r��@r~�@r�@qhs@q&�@p��@p�u@pQ�@o�;@ol�@nȴ@n��@n��@nff@n@m�@m��@mV@lI�@k�m@k"�@j��@j~�@j=q@i�#@i�7@h�`@h1'@g�w@g;d@fȴ@fff@fE�@f5?@e@e�@d�j@d(�@c�
@cC�@b��@b��@bM�@a��@a��@a��@`��@`A�@_�@^ȴ@^ff@^{@^@]�T@]��@]��@]�h@]/@\��@\�D@[��@[t�@[C�@[33@Z�@Z~�@Y��@Y��@YX@Y%@X��@XĜ@X�u@XbN@XQ�@X1'@W�@W��@W�@W�P@Wl�@W\)@WK�@W�@V�y@Vȴ@V��@U`B@UV@Tz�@T(�@Sƨ@S��@S��@SS�@R�H@R�\@R�@Q�7@Q&�@P�@P  @O��@O\)@N��@N�R@Nv�@NE�@N$�@N$�@N@M�T@M��@MO�@L��@L�D@Lj@LI�@K��@Kt�@K33@J��@JJ@I��@I&�@I%@I%@I%@H��@H�9@H1'@H  @G�@G��@G��@G;d@F�@Fff@E@E�-@E��@E�@D��@D�D@DZ@D1@C�F@C�@Ct�@C33@Bn�@A�^@A�@@�`@@�u@@A�@@b@?�;@?��@?K�@?+@>�y@>E�@=�@=�-@=�@=/@=�@<��@<��@<�@<�D@<9X@<1@;�
@;��@;S�@;33@;"�@;"�@:�H@:��@:n�@:^5@:=q@:=q@:-@:J@9��@9x�@9G�@9�@9%@8��@81'@8  @7��@7�@7|�@7+@6�@6V@6@5�T@5@5�h@5O�@5/@4�@4��@4Z@3��@3�
@3ƨ@3��@3t�@333@2�H@2�\@2n�@2M�@1�@1�7@1hs@1&�@0��@0�9@01'@0  @/��@/�@/�P@/\)@.��@.�R@.��@.v�@.5?@.{@.@-�@-@-p�@-/@-/@-V@,�@,z�@,I�@,�@+�m@+ƨ@+ƨ@+ƨ@+�F@+�F@+��@+dZ@+C�@*��@*M�@*J@)�@)�#@)��@)�7@)&�@(��@(��@(�@(bN@(Q�@(A�@(b@'�;@'�@'�P@'K�@&�y@&��@&E�@&$�@&@%�T@%��@%��@%@%�@%/@$�@$��@$Z@$�@#�m@#��@#�@#dZ@#C�@#"�@"��@"n�@"M�@"=q@"-@"-@"J@!��@!��@!x�@!7L@!�@!%@ �9@ �u@ Q�@  �@�;@�P@�@�y@ȴ@�+@E�@{@@�h@p�@�@��@�D@j@Z@�@��@�m@�F@dZ@S�@dZ@dZ@S�@dZ@C�@C�@o@~�@^5@^5@M�@�@J@J@��@hs@X@G�@�@��@��@�`@�`@��@Ĝ@bN@ �@��@l�@K�@
=@��@ȴ@��@v�@v�@ff@5?@@@�h@`B@�@V@��@j@(�@1@1@�m@��@t�@dZ@S�@"�@��@�\@~�@~�@^5@J@�^@��@hs@X@G�@7L@��@��@Ĝ@�9@�9@�@1'@��@��@l�@l�@\)@;d@�@V@@�@��@�h@`B@O�@/@�@�j@�@��@�D@z�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�z�A�~�AʁA�z�AʁAʃAʅAʇ+Aʇ+AʃA�v�A�t�A�t�A�r�A�hsA�ffA�ffA�ffA�`BA�VA�-A�ƨAȕ�A�ZA�v�AŲ-A�{A�G�A�`BA�l�A�=qA���A�Q�A�7LA�^5A���A�(�A�ffA��TA�v�A�~�A�
=A� �A�v�A�VA�?}A�A��^A���A��7A�Q�A���A��-A��A�v�A�?}A��A�oA��TA��A�{A�ĜA�-A���A�A�A��A�jA��9A���A�n�A�33A���A�|�A���A���A� �A��A���A��#A�$�A��A�"�A�t�A�p�A�C�A�Q�A���A�ffA�bA���A�{A���A��A��FA��DA�hsA�I�A�1A��-A�1'A���A�5?A�^5A�XA��FA�~�A��A��A�n�A��hA�jA���A���A�n�A��HA�&�A���A��`A���A�1AO�A~ZA}|�A|��A{�Ay�
Aw�wAsdZAq��Ao��Al�HAi�#Ad�\Ab�A`VA^�yA]��A\��A[�FAZ�uAX�+AWl�AV�9ARz�AP�uAL�AIx�AH�AG��AGXAGC�AF�AD�jAB�AA�A@��A@{A>(�A=�A;�A;VA:bNA9��A9��A9dZA9"�A8�RA8M�A7�7A5�-A4ffA3�
A2��A1?}A/��A/oA.�A.-A-��A,�/A*��A(^5A'/A%�A#�A"��A"E�A!XA �yA ��A ZA�A��AA�7A��An�A��AXAAbAĜA��A�TA�A�HAv�A�^A�A�AoAp�A
VAȴAdZAJA��A1'A�A�;AoA {@�/@���@��T@��9@�?}@��`@�j@�!@�7@��`@��@��@�?}@���@�z�@��@���@�9@�Z@��;@���@�-@��@���@�bN@�9X@�(�@�F@��@���@�Z@�1@���@݉7@�r�@�Q�@���@�33@�&�@���@Չ7@��@��;@�x�@�ƨ@�-@̼j@ˍP@ʟ�@�p�@�%@ȣ�@�j@�(�@š�@�bN@�
=@�@�v�@�E�@��@���@�p�@�?}@���@���@���@���@�@��y@��#@�ff@�v�@�5?@�/@���@�9X@�S�@��\@�x�@��u@� �@�K�@���@��#@���@��@�  @�l�@�33@��!@�=q@�x�@�%@���@���@��
@�C�@�^5@��7@�X@��@��@�j@���@�S�@��y@���@�-@��T@���@�?}@���@�Q�@�1'@�b@��;@��F@�;d@�@���@��\@�^5@��@�X@��`@��j@��D@�j@�Q�@� �@��w@�ƨ@��F@�|�@�
=@��R@��@�@��@�X@�p�@�?}@�Ĝ@�9X@���@��@��y@���@�ff@�-@�@��#@��7@��@���@��/@��D@�1@��F@���@�S�@���@���@��R@���@�^5@�-@�@���@��-@��@���@�z�@�1'@��m@��w@���@�33@�o@�ȴ@��+@�-@���@��^@��7@�p�@�`B@�G�@��@�V@��@��j@���@�Q�@� �@��
@���@�l�@�33@�o@��H@���@���@�v�@�M�@�-@�-@�5?@�-@�-@�J@�@�X@�p�@�x�@�X@�/@��@��9@��@��@��w@�1@�I�@�(�@�(�@�b@���@��H@��R@��\@�J@��h@�7L@�%@���@� �@��m@�|�@�33@�@�ȴ@���@��\@�E�@��T@���@���@�G�@�&�@��/@�Ĝ@�r�@�I�@�I�@��w@�t�@�33@��@�@�@���@�ȴ@�ȴ@��\@�v�@�=q@�=q@�-@�$�@�{@��@��^@��h@�O�@��@�Ĝ@��D@��@�z�@�bN@�Q�@�Q�@�I�@�9X@�1@��@�t�@�S�@�\)@�|�@��@�t�@�K�@�@���@���@���@�~�@�v�@�v�@�V@�=qG�O�@��h@�?}@�7L@�&�@�&�@��@���@���@��@�Q�@�(�@��@��@\)@
=@~�@~�+@~@}@}?}@|j@{ƨG�O�@y��@x��@xQ�@xb@w�@w|�@v��@v�R@v�R@v�+@u�h@uV@t��@tZ@t(�@t�@s�
@so@r��@r~�@r�@qhs@q&�@p��@p�u@pQ�@o�;@ol�@nȴ@n��@n��@nff@n@m�@m��@mV@lI�@k�m@k"�@j��@j~�@j=q@i�#@i�7@h�`@h1'@g�w@g;d@fȴ@fff@fE�@f5?@e@e�@d�j@d(�@c�
@cC�@b��@b��@bM�@a��@a��@a��@`��@`A�G�O�@^ȴ@^ff@^{@^@]�T@]��@]��@]�h@]/@\��@\�D@[��@[t�@[C�@[33@Z�@Z~�@Y��@Y��@YX@Y%@X��@XĜ@X�u@XbN@XQ�@X1'@W�@W��@W�@W�P@Wl�@W\)@WK�@W�@V�y@VȴG�O�@U`B@UV@Tz�@T(�@Sƨ@S��@S��@SS�@R�H@R�\@R�@Q�7@Q&�@P�@P  @O��@O\)@N��@N�R@Nv�@NE�@N$�@N$�@N@M�T@M��@MO�@L��@L�D@Lj@LI�@K��@Kt�@K33@J��@JJ@I��@I&�@I%@I%@I%@H��@H�9@H1'@H  @G�@G��@G��@G;d@F�@Fff@E@E�-@E��@E�@D��@D�D@DZ@D1@C�F@C�@Ct�G�O�@Bn�@A�^@A�@@�`@@�u@@A�@@b@?�;@?��@?K�@?+@>�y@>E�@=�@=�-@=�@=/@=�@<��@<��@<�@<�D@<9X@<1@;�
@;��@;S�@;33@;"�@;"�@:�H@:��@:n�@:^5@:=q@:=q@:-@:J@9��@9x�@9G�@9�@9%@8��@81'@8  @7��@7�@7|�@7+@6�@6V@6@5�T@5@5�h@5O�@5/@4�@4��@4Z@3��@3�
@3ƨ@3��@3t�@333@2�H@2�\@2n�@2M�@1�@1�7@1hs@1&�@0��@0�9@01'@0  @/��@/�@/�P@/\)@.��@.�R@.��@.v�@.5?@.{@.@-�@-@-p�@-/@-/@-V@,�@,z�@,I�@,�@+�m@+ƨ@+ƨ@+ƨ@+�F@+�F@+��@+dZ@+C�@*��@*M�@*J@)�@)�#@)��@)�7@)&�@(��@(��@(�@(bN@(Q�@(A�@(b@'�;@'�@'�P@'K�@&�y@&��@&E�@&$�@&@%�T@%��@%��@%@%�@%/@$�@$��@$Z@$�@#�m@#��@#�@#dZ@#C�@#"�@"��@"n�@"M�@"=q@"-@"-@"J@!��@!��@!x�@!7L@!�@!%@ �9@ �u@ Q�@  �@�;@�P@�@�y@ȴ@�+@E�@{@@�h@p�@�@��@�D@j@Z@�@��@�m@�F@dZ@S�@dZ@dZ@S�@dZ@C�@C�@o@~�@^5@^5@M�@�@J@J@��@hs@X@G�@�@��@��@�`@�`@��@Ĝ@bN@ �@��@l�@K�@
=@��@ȴ@��@v�@v�@ff@5?@@@�h@`B@�@V@��@j@(�@1@1@�m@��@t�@dZ@S�@"�@��@�\@~�@~�@^5@J@�^@��@hs@X@G�@7L@��@��@Ĝ@�9@�9@�@1'@��@��@l�@l�@\)@;d@�@V@@�@��@�h@`B@O�@/@�@�j@�@��@�D@z�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111131111111111111111111111111111111111111111111111111113111111113111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111141111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B
(�B
'�B
&�B
(�B
(�B
(�B
(�B
(�B
(�B
'�B
(�B
(�B
(�B
'�B
)�B
)�B
)�B
)�B
(�B
'�B
&�B
6FB
�fBXB{�B��B�B�XB��B�B��B	7B\B	7BhB#�B&�B$�B �B49BF�BD�BJ�BXBZBXBYB^5B]/B[#BZB_;BffBq�B{�B�B�+B��B��B�uB�oB�hB��B��B��B�{B��B��B��B��B��B�PB�VB�VB�+B~�By�Bq�BjBYBO�BP�B@�B+B�B��B�fB�;B��B��B�dBy�Bx�Bt�Bp�Bl�BgmB`BBT�BF�B1'B{B�B�B�BuBVB
��B
�NB
ɺB
�dB
��B
�=B
q�B
ffB
R�B
N�B
?}B
=qB
<jB
6FB
.B
'�B
�B
DB	�B	��B	�
B	ĜB	�-B	�hB	x�B	~�B	n�B	o�B	jB	e`B	^5B	VB	E�B	@�B	:^B	�B	{B��B�B	B	B	  B��B��B�fB�)B�BB�B��BȴBȴBƨBŢBĜBƨBƨBŢBÖB�}B�dB�9B��B��B��B��B��B��B��B��B��B��B�DB|�Bp�Bx�Bp�Bt�Bw�Bz�Bw�By�B{�Bt�BiyBhsBo�Be`Bn�Bl�Bk�Bk�BhsB`BBW
BYBO�BI�BZB`BBZBXBS�BS�BJ�BM�BI�BM�BL�BQ�BT�BS�BO�BS�BR�BK�BT�BR�BR�BL�B_;B^5BW
B[#B^5B^5BVB_;BcTB`BBYBbNBjBjBiyBgmBl�Bm�Bm�Bo�Br�Br�Bo�Bk�BiyBgmBp�Bp�Bv�Bw�B|�By�Bv�Bp�Bq�Bw�B}�Bx�Bu�B{�B�B�B�7B�\B�bB��B��B��B�uB�DB��B��B��B�B�B�B�B�'B�-B�-B�9B�?B�FB�LBBĜB��B��B��B��B��B�B�
B�B�5B�HB�`B�ZB�ZB�B�B��B��B��B��B��B	  B	B	B	%B	B	B		7B		7B	bB	�B	�B	�B	�B	�B	#�B	'�B	.B	1'B	49B	6FB	7LB	9XB	?}B	C�B	D�B	E�B	G�B	G�B	L�B	O�B	O�B	O�B	O�B	O�B	T�B	ZB	ZB	[#B	[#B	_;B	cTB	gmB	hsB	hsB	iyB	l�B	iyB	p�B	r�B	u�B	v�B	x�B	x�B	y�B	}�B	�B	�B	�B	�%B	�+B	�1B	�7B	�=B	�JB	�oB	�uB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�3B	�9B	�9B	�FB	�LB	�RB	�XB	�jB	�qB	�}B	��B	��B	B	B	ÖB	ÖB	ÖB	ĜB	ŢB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�#B	�#B	�)B	�HB	�TB	�TB	�ZB	�ZB	�`B	�fB	�`B	�mB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
+B
+B
1B
1B

=B
JB
JB
JB
PB
PB
PB
JB
JB
DB
PB
\B
hB
uB
uB
oB
oB
oB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
�B
�B
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
!�B
 �B
�B
 �B
!�B
!�B
"�B
$�B
$�B
#�B
#�B
#�B
#�B
$�B
&�B
(�B
(�B
'�B
(�B
(�B
'�B
&�B
(�B
(�B
(�B
)�B
)�B
(�B
)�B
(�B
)�B
+B
+B
+B
,B
,B
,B
+B
+B
+B
,B
-B
,B
-B
.B
.B
/B
/B
/B
.B
-B
.B
,B
-B
.B
1'B
2-B
2-B
2-B
1'B
0!B
0!B
0!B
0!B
0!B
1'B
2-B
1'B
1'B
0!B
2-B
2-B
2-B
2-B
33B
33B
33B
33B
33B
2-B
33B
33B
33B
33B
33B
33B
33B
2-B
33B
2-B
/B
33B
2-B
49B
49B
5?B
6FB
5?B
5?B
6FB
6FB
6FB
7LB
6FB
7LB
8RB
8RB
8RB
:^B
:^B
;dB
;dB
<jB
<jB
<jB
<jB
<jB
;dB
;dB
<jB
<jB
;dB
:^B
<jB
<jB
<jB
>wB
>wB
@�B
@�B
@�B
@�B
?}B
>wB
@�B
A�B
A�B
B�B
A�B
@�B
A�B
A�B
D�B
D�B
C�B
B�B
C�B
D�B
C�B
D�B
E�B
E�B
D�B
B�B
D�B
E�B
G�B
G�B
G�B
H�B
H�B
H�B
H�B
I�B
H�B
H�B
I�B
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
L�B
K�B
L�B
L�B
M�B
M�B
L�B
L�B
M�B
M�B
M�B
N�B
M�B
M�B
M�B
M�B
M�B
N�B
N�B
M�B
M�B
N�B
O�B
O�B
O�B
N�B
N�B
N�B
O�B
P�B
Q�B
P�B
P�B
Q�B
Q�B
P�B
Q�B
Q�B
R�B
S�B
S�B
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
VB
VB
VB
VB
VB
VB
W
B
XB
XB
XB
YB
YB
YB
XB
XB
YB
YB
YB
YB
XB
YB
ZB
ZB
ZB
[#B
[#B
[#B
[#B
[#B
ZB
ZB
ZB
ZB
[#B
[#B
\)B
[#B
[#B
[#B
[#B
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
_;B
_;B
`BB
`BB
`BB
`BB
`BB
`BB
_;B
^5B
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
bNB
cTB
cTB
bNB
bNB
bNB
bNB
cTB
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
gmB
hsB
hsB
hsB
hsB
hsB
gmB
iyB
iyB
iyB
iyB
iyB
iyB
iyB
hsB
gmB
iyB
iyB
iyB
iyB
iyB
iyB
iyB
hsB
jB
jB
jB
jB
k�B
k�B
k�B
jB
jB
iyB
jB
jB
k�B
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
m�B
n�B
n�B
n�B
m�B
o�B
o�B
p�B
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
q�B
p�B
p�B
q�B
r�B
r�B
r�B
r�B
r�B
r�B
s�B
s�B
s�B
r�B
r�B
r�B
s�B
s�B
t�B
t�B
s�B
s�B
s�B
t�B
u�B
u�B
u�B
u�B
v�B
v�B
v�B
v�B
w�B
w�B
w�B
w�B
w�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B
(�B
(
B
'B
(�B
(�B
(�B
)B
(�B
)B
(
B
(�B
(�B
(�B
(
B
)�B
)�B
*B
*B
)DB
(�B
(sB
:�B
�XBY�B}�B�B��B�JB�vB�B�B	�B�BB&B%B)*B(�B%`B6+BG�BFtBK�BX_BZ�BX�BY�B^jB]~B[�BZ�B_�Bf�Bq�B|PB�aB��B��B�]B��B�[B��B��B�B��B��B�?B�yB�hB��B�B��B�vB�BB�fB��B{�Bs�BlqB\BQ�BR�BB�B-�BB��B�KB��BÖB��B�;B}VByXBu%BqBmBh$Ba-BV�BH�B4�ByB�B	B=BaBBB �B
�B
��B
�B
�fB
�BB
u�B
jKB
VB
QhB
BAB
>�B
=qB
7fB
/5B
)DB
;B
"B	��B	��B	�eB	�zB	��B	��B	~wB	�;B	q�B	q[B	lB	f�B	_�B	W�B	HB	B'B	<jB	�B	�B	�B��B	B	�B	 �B�}B��B�DBބB�BیBևB�B�=B�1B��BŢB�EB�EB�B�B�OB�PB��B�6B��B��B�|B�WB�#B��B�dB�kB�sB�B�Bs�Bz�Bs3Bv`By>B{�Bx�BzxB|jBu�Bk�BjBp�BgRBo�Bm]BlqBl=BiDBa�BX�BZ�BR�BL0B[�BaB[WBYeBU�BUgBMBO�BK�BO�BN�BSuBVBU�BQ�BUMBT{BM�BVBTaBT,BN�B_�B^�BX_B[�B^�B^�BW�B_�Bc�B`�BZ�Bb�Bj�BkBjBhXBl�Bm�BnBpBr�Br�Bp!Bl=BjBhsBq'Bq�Bw�Bx�B}"Bz^Bw�BrGBsBx�B~�By�Bw�B}"B�'B�9B�#B�.B�4B��B��B��B�B��B��B��B�KB�]B�cB�wB�cB�[B�|B��B��B��B��B�8B��B�9B̳B�B�uBѷBՁBևB��B��B�!B��B��B�,B�`B��B�AB�LB�>B�^B�<B�]B	 iB	�B	aB	?B	�B	�B		�B		�B	 B	�B	�B	B	B	;B	$@B	(XB	.cB	1vB	4�B	6zB	7�B	9�B	?�B	C�B	D�B	E�B	G�B	HB	MB	PB	PB	P.B	PbB	P}B	UgB	Z7B	ZQB	[WB	[WB	_pB	c�B	gmB	h�B	h�B	i�B	l�B	i�B	p�B	r�B	u�B	v�B	y	B	y>B	z^B	~wB	�GB	��B	�SB	�tB	�zB	��B	�lB	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	�B	�B	� B	�&B	�B	�B	�DB	�yB	�]B	�iB	�OB	�oB	�hB	��B	��B	�zB	��B	��B	��B	��B	��B	��B	��B	��B	ªB	ªB	ðB	ðB	��B	��B	��B	��B	��B	��B	�B	�B	��B	�B	�B	�B	�B	�B	�B	��B	�
B	�B	�7B	�=B	�qB	ܒB	�bB	�TB	�B	�B	�B	�B	�B	�B	�B	�kB	�}B	��B	�B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	�B	�%B	�B	�B	�8B	��B	�6B	�B	�6B	�B	�(B	�PB	�<B	�HB
 B
AB
-B
GB
-B
B
GB
3B
3B
B
3B
SB
SB
SB
YB
_B
_B
fB
�B

rB
JB
dB
~B
jB
PB
�B
~B
�B
�B
�B
vB
hB
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�G�O�B
�B
�B
�B
�B
�B
�B
�B
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
B
/G�O�B
	B
�B
�B
�B
�B
�B
�B
�B
B
B
B
 �B
�B
!�B
!�B
!B
 B
 �B
!�B
!�B
#B
$�B
$�B
$&B
#�B
$B
$B
%B
'B
)B
)B
(
B
)B
)DB
($B
'8B
)*B
)_B
)*B
*B
*B
)*B
*0B
)DB
*KB
+6B
+6B
+QB
,"B
,=B
,"B
+QB
+6B
+6B
,=B
-CB
,WB
-CB
.IB
./B
/5B
/OB
/OB
.cB
-]B
.}G�O�B
-]B
.IB
1'B
2-B
2-B
2aB
1[B
0UB
0UB
0;B
0oB
0oB
1AB
2aB
1[B
1[B
0oB
2aB
2aB
2aB
2GB
3MB
3MB
3MB
3MB
3hB
2GB
3hB
3hB
3MB
3MB
33B
3hB
3MB
2aB
3MB
2�G�O�B
3MB
2aB
4TB
4TB
5ZB
6`B
5�B
5tB
6zB
6�B
6zB
7�B
6zB
7�B
8lB
8�B
8�B
:xB
:�B
;B
;B
<jB
<�B
<�B
<�B
<�B
;�B
;�B
<�B
<�B
;�B
:�B
<�B
<�B
<�B
>�B
>�B
@�B
@�B
@�B
@�B
?�B
>�B
@�B
A�B
A�B
B�B
A�B
@�B
A�B
A�B
D�B
D�B
C�B
B�B
C�B
D�B
C�B
D�B
E�B
E�B
D�G�O�B
D�B
E�B
G�B
G�B
G�B
H�B
H�B
H�B
H�B
I�B
H�B
H�B
I�B
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
MB
K�B
MB
L�B
M�B
M�B
MB
MB
M�B
M�B
NB
N�B
NB
M�B
M�B
M�B
NB
N�B
N�B
NB
NB
N�B
O�B
O�B
O�B
OB
OB
O(B
O�B
Q B
RB
QB
QB
RB
R B
QB
RB
R B
S&B
TB
TB
S&B
S&B
S&B
T,B
TB
T,B
TFB
TB
UB
UB
UB
U2B
U2B
V9B
V9B
V9B
V9B
VB
V9B
W$B
XEB
X+B
X+B
Y1B
YB
YKB
X+B
X+B
YKB
YB
Y1B
Y1B
XEB
YKB
Z7B
ZQB
ZQB
[=B
[#B
[#B
[=B
[=B
Z7B
Z7B
ZQB
ZQB
[WB
[WB
\CB
[=B
[=B
[WB
[=B
\]B
\CB
\]B
]IB
]IB
^jB
^jB
^jB
^OB
^OB
^�B
_pB
_VB
`vB
`vB
`\B
`BB
`BB
`vB
_pB
^jB
_VB
_pB
_pB
`vB
`vB
`vB
aHB
a|B
a|B
abB
abB
abB
bhB
cTB
cnB
bNB
bhB
b�B
bhB
cnB
b�B
cnB
c�B
cnB
c�B
cnB
c�B
c�B
c�B
c�B
dtB
dtB
dtB
dtB
e�B
e�B
ezB
ezB
e�B
f�B
f�B
g�B
hsB
h�B
h�B
h�B
h�B
g�B
i�B
iyB
i�B
iyB
iyB
i�B
i�B
h�B
g�B
iyB
iyB
iyB
i�B
i�B
iyB
i�B
h�B
jB
j�B
j�B
j�B
k�B
k�B
k�B
jB
j�B
i�B
j�B
j�B
k�B
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
m�B
n�B
n�B
n�B
m�B
o�B
o�B
p�B
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
q�B
p�B
p�B
q�B
r�B
r�B
r�B
r�B
r�B
r�B
s�B
s�B
s�B
r�B
r�B
r�B
s�B
s�B
t�B
t�B
s�B
s�B
s�B
t�B
u�B
u�B
u�B
u�B
v�B
v�B
v�B
v�B
w�B
w�B
w�B
w�B
w�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111131111111111111111111111111111111111111111111111111113111111113111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111141111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.08(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201805180036452018051800364520180518003645201806221330302018062213303020180622133030201806042132562018060421325620180604213256  JA  ARFMdecpA19c                                                                20180514093550  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180514003611  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180514003614  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180514003615  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180514003615  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180514003615  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180514003615  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180514003615  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20180514003616  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20180514003616                      G�O�G�O�G�O�                JA  ARUP                                                                        20180514005713                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180514153254  CV  JULD            G�O�G�O�F��                JM  ARSQJMQC2.0                                                                 20180515000000  CF  PSAL_ADJUSTED_QCC8  D�� G�O�                JM  ARSQJMQC2.0                                                                 20180515000000  CF  TEMP_ADJUSTED_QCC8  D�� G�O�                JM  ARCAJMQC2.0                                                                 20180517153645  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180517153645  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180604123256  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622043030  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200116221515                      G�O�G�O�G�O�                