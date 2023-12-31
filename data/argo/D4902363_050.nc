CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2016-10-24T00:35:24Z creation;2016-10-24T00:35:26Z conversion to V3.1;2019-12-19T08:27:10Z update;     
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
resolution        =���   axis      Z        \  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  H�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     \  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \,   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     \  `   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o`   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     \  s8   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     \  �l   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     \  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     \  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     \  �0   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     \  ˌ   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
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
_FillValue                    �xArgo profile    3.1 1.2 19500101000000  20161024003524  20200115111517  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               2A   JA  I2_0576_050                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @�Ե�o��1   @�Զ��À@:�s�PH�d�:��1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@ffBH  BO��BX  B`  Bh  Bp  Bx  B�  B�  B�  B�33B�33B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@�CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD fD � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dg��Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�<�D׀ D�� D�  D�C3D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D��3D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�3D�@ D� D�� D�  D�@ D� D��D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D��3D��3D�f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��
@�p�@�p�A�RA>�RA^�RA~�RA�\)A�\)A�\)A�\)A�\)A�\)A�\)A�\)B�B�B�B�B'�B/�B7�B@{BG�BOG�BW�B_�Bg�Bo�Bw�B�B��
B��
B�
=B�
=B��
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
B��
B��
B��
B��
B��
B��
C�C�C�C�C	�C�C�C�C�C�C�C�C�C�C�C�C!�C#�C%�C'�C)�C+�C-�C/�C1�C3�C5�C7�C9�C;�C=�C@CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D z�D ��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��D	z�D	��D
z�D
��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��D�HD HD z�D ��D!z�D!��D"z�D"��D#z�D#��D$z�D$��D%z�D%��D&z�D&��D'z�D'��D(z�D(��D)z�D)��D*z�D*��D+z�D+��D,z�D,��D-z�D-��D.z�D.��D/z�D/��D0z�D0��D1z�D1��D2z�D2��D3z�D3��D4z�D4��D5z�D5��D6z�D6��D7z�D7��D8z�D8��D9z�D9��D:z�D:��D;z�D;��D<z�D<��D=z�D=��D>z�D>��D?z�D?��D@z�D@��DAz�DA��DBz�DB��DCz�DC��DDz�DD��DEz�DE��DFz�DF��DGz�DG��DHz�DH��DIz�DI��DJz�DJ��DKz�DK��DLz�DL��DMz�DM��DNz�DN��DOz�DO��DPz�DP��DQz�DQ��DRz�DR��DSz�DS��DTz�DT��DUz�DU��DVz�DV��DWz�DW��DXz�DX��DYz�DY��DZz�DZ��D[z�D[��D\z�D\��D]z�D]��D^z�D^��D_z�D_��D`z�D`��Daz�Da��Dbz�Db��Dcz�Dc��Ddz�Dd��Dez�De��Dfz�Df��Dgz�Dg�{Dhz�Dh��Diz�Di��Djz�Dj��Dkz�Dk��Dlz�Dl��Dmz�Dm��Dnz�Dn��Doz�Do��Dpz�Dp��Dqz�Dq��Drz�Dr��Dsz�Ds��Dtz�Dt��Duz�Du��Dvz�Dv��Dwz�Dw��Dxz�Dx��Dyz�Dy��Dzz�Dz��D{z�D{��D|z�D|��D}z�D}��D~z�D~��Dz�D��D�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��=D��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��=D��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD½qD��qD�=qD�}qDýqD��qD�=qD�}qDĽqD��qD�=qD�}qDŽqD��qD�=qD�}qDƽqD��qD�=qD�}qDǽqD��qD�=qD�}qDȽqD��qD�=qD�}qDɽqD��qD�=qD�}qDʽqD��qD�=qD�}qD˽qD��qD�=qD�}qD̽qD��qD�=qD�}qDͽqD��qD�=qD�}qDνqD��qD�=qD�}qDϽqD��qD�=qD�}qDнqD��qD�=qD�}qDѽqD��qD�=qD�}qDҽqD��qD�=qD�}qDӽqD��qD�=qD�}qDԽqD��qD�=qD�}qDսqD��qD�=qD�}qDֽqD��qD�:=D�}qD׽qD��qD�@�D�}qDؽqD��qD�=qD�}qDٽqD��qD�=qD�}qDڽqD��qD�=qD�}qD۽qD��qD�=qD�}qDܽqD��qD�=qD�}qDݽqD��qD�=qD�}qD޽qD��qD�=qD�}qD߽qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD���D��qD�=qD�}qD��qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD� �D�=qD�}qD�qD��qD�=qD�}qD�=D��qD�=qD�}qD�qD��qD�=qD�}qD��qD��qD�=qD���D���D��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A��
A��
A��
A��
A��
A��
A��A��;A��/A��HA��TA��TA��HA��HA��/A��HA��HA��;A��#A��
A�A�z�A�VA���A�7LA��/A�?}A��TA�ĜAыDA��AЕ�A�"�A�K�AɁA�dZA� �A���A��^A�?}A�p�A�ȴA�|�A���A��A�\)A���A�+A� �A�
=A�jA�dZA���A�O�A���A�bA�  A�p�A��jA�~�A�(�A��TA��
A�ĜA��7A���A�~�A��-A�ȴA�-A�-A�
=A��A�VA��/A��uA�C�A�l�A�  A�~�A��A~A�A|�\Az��Awl�AuhsAt�!As��AshsAr��Ar��Ar=qApffAo
=An�+AnQ�AnAm�Ak�;Ak�PAkO�Aj��Aj�uAj~�Ajr�AjI�Ai�PAi+Ah9XAfQ�Ac��AcXAcO�Ab��Ab-Aa`BA`-A^�9A^ �A]��A]"�A\�/A\�A[�FA[oAZ�AYƨAY+AX��AXjAXI�AV�DAU�mAUp�AS�PAR��AO��AL��AK�7AJ1AIhsAH��AFJAB�AA|�A@��A@~�A@5?A?��A>�\A=��A<�9A<(�A<1A;�#A;+A:�\A:$�A9��A9�A9S�A8�DA7��A7+A6��A6E�A5�wA4��A3�PA2��A2~�A2 �A1�A0�A0E�A/�A/ƨA/S�A.I�A-S�A,�yA,n�A+��A*�A)K�A(�A'�-A'x�A&�jA&JA%`BA#��A"��A"�A!hsA!G�A!+A!"�A �A $�A�A��Al�A��AI�A��AA�+AbA�FA&�A��Ap�A=qAJA�A�AjAQ�A$�A��A �A{A��AA=qAn�A;dA�!At�A
ȴA
A��A��A  A�7A��A�+A�A�yA  A �A bN@��@�\)@��\@���@�~�@���@��@�V@��y@�@���@���@���@���@�7L@��@�hs@�;d@�@�ƨ@�R@��@��@�bN@�A�@��@ޏ\@�ƨ@�p�@�&�@�1'@ם�@ׅ@ׅ@ו�@�K�@�n�@�9X@҇+@�O�@υ@��@�5?@��@ǶF@ư!@��@Ĭ@�j@�Q�@���@���@�V@��T@��-@�O�@��D@���@�+@���@��h@�  @�;d@��h@�t�@�Ĝ@��D@�9X@��@��@�&�@��`@��@�"�@���@���@��@��`@��P@�V@��^@�x�@��@��@��@��@���@�  @���@�t�@�C�@�@�^5@��@�O�@��`@�z�@� �@��w@���@�33@��@���@�=q@��^@�`B@��/@�z�@�r�@�r�@�j@�j@�Z@� �@��@���@��#@���@��@�Z@�;d@��h@���@���@���@�Q�@��@��@�"�@���@��\@�M�@�{@�?}@�1@�@���@�=q@��@�@��h@�p�@��@��@�j@�  @�t�@��H@��+@�5?@��T@���@�X@�&�@�Ĝ@�Z@�I�@�(�@�1@���@��@��P@�dZ@�+@���@��!@�n�@��@���@���@���@�O�@���@��u@�A�@��@�ƨ@���@��P@�t�@�S�@�33@�o@���@�n�@�{@���@��^@��-@��@�/@���@���@�z�@�j@�Q�@�I�@�I�@�9X@��@�P@~��@~ff@~{@}��@}`B@}O�@}O�@}�@|�j@|�D@|j@|I�@|Z@|j@|(�@{dZ@{"�@{"�@{"�@zn�@x�u@wl�@v��@v5?@u��@t��@tZ@tz�@t9X@t9X@tZ@st�@s"�@so@s@rn�@qx�@q%@pQ�@o��@o�@o�P@ol�@o+@o
=@n��@n�@n�R@n��@nv�@nff@nE�@nE�@n$�@m�@m@m�@m�-@m��@m`B@m/@m/@m?}@m/@mV@mV@l��@mV@l��@l��@l�@k�
@k33@j��@j^5@j=q@j=q@j�@i��@i�@h�`@h�`@h�9@h�u@hA�@g�;@g;d@f�@f�y@f�@f��@f��@fff@e�T@e@f@f@f@f@f@f@f{@f{@e�@e��@e@e�@d�@c�
@c33@b��@bJ@a��@a�@`�`@`�9@`��@`bN@`A�@`b@_��@_l�@^�y@^�+@_;d@_K�@_
=@^�y@^ȴ@^��@^v�@^V@^5?@^$�@]��@]`B@\��@\Z@[�F@[t�@Z�H@Y��@Y�@Y�#@Y��@Y��@YX@Yhs@YX@Y7L@Y&�@Y&�@X��@Xr�@X  @W�@W�@Wl�@V�y@V�@VE�@UV@Tj@S��@SS�@S@R~�@Q�#@Q��@QG�@P��@P �@O�@O��@O�P@OK�@N��@N@M�@M/@L�/@L�D@LI�@L(�@K�F@J�H@J��@Jn�@J-@I�#@I�7@I%@H�u@HbN@HA�@Hb@Gl�@F�R@F@E@E�-@E�@EO�@D�@Dj@C�F@C��@C�@Ct�@Ct�@Ct�@C�@CdZ@C33@C"�@B��@B=q@A�@A��@Ax�@AG�@A�@A%@A%@A%@@�`@@��@@Q�@@b@@  @?�w@?+@>ȴ@>��@>��@>ff@>$�@=�@=@=p�@=V@<�j@<�@<��@<��@<�D@<�D@<j@<(�@;�m@;t�@;o@:��@:�\@:~�@:M�@:�@9��@9�^@9X@9�@8��@8��@8�9@8�@8bN@8A�@8 �@8  @7�w@7�P@7l�@7;d@7
=@6�@6��@6�+@6V@6E�@6{@5�@5@5��@5p�@5V@4��@4��@4��@4�j@4�@4j@4�@3��@3"�@3o@3o@3@2��@2-@1��@1x�@1X@1G�@1�@0��@0bN@/�;@/�P@/\)@/;d@/�@.ȴ@.��@.ff@.5?@.@-�T@-��@-�-@-�h@-`B@-�@,�@,�D@,9X@,�@,1@+�
@+33@+"�@*�@*^5@)��@)x�@)G�@(��@(�9@(�u@(�u@(�@(bN@(A�@(  @'�P@'�@&��@&V@&@%��@%�-@%��@%�h@%�h@%?}@%�@$�@$��@$�@$�D@$j@$9X@#��@#ƨ@#dZ@#@"n�@"M�@"J@!��@!��@!��@!7L@ ��@ bN@ 1'@�@�w@�@��@K�@+@
=@�R@��@�h@O�@�@�/@�j@�j@��@j@�@��@C�@"�@�@n�@-@�7@��@Ĝ@�u@bN@�w@\)@K�@+@�@�y@ȴ@�R@�+@v�@5?@�-@p�@V@�/@�@z�@j@Z@I�@9X@ƨ@C�@�@��@�!@��@n�@M�@�@�@��@�@�@�^@��@%@��@��@�9@�9@��@�@1'@�@��@��@�w@�P@l�@\)@;d@
=@��@��@�y@�y@�@�@ȴ@�R@�R@�+@V@E�@5?@5?@$�@�@�T@@�-@��@��@�@p�@`B@`B@?}@�@V@��@�@�D@j@I�@I�@I�@9X@1@��@�@�@t�@S�@C�@
�H@
�\@
M�@
M�@
-@
�@	�@	�#@	�#@	�#@	��@	��@	X@	%@��@Ĝ@��@�@bN@A�@1'@ �@  @l�@+@
=@�y@�y@�y@�@ȴ@�R@�+@�+@ff@@��@@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A��
A��
A��
A��
A��
A��
A��A��;A��/A��HA��TA��TA��HA��HA��/A��HA��HA��;A��#A��
A�A�z�A�VA���A�7LA��/A�?}A��TA�ĜAыDA��AЕ�A�"�A�K�AɁA�dZA� �A���A��^A�?}A�p�A�ȴA�|�A���A��A�\)A���A�+A� �A�
=A�jA�dZA���A�O�A���A�bA�  A�p�A��jA�~�A�(�A��TA��
A�ĜA��7A���A�~�A��-A�ȴA�-A�-A�
=A��A�VA��/A��uA�C�A�l�A�  A�~�A��A~A�A|�\Az��Awl�AuhsAt�!As��AshsAr��Ar��Ar=qApffAo
=An�+AnQ�AnAm�Ak�;Ak�PAkO�Aj��Aj�uAj~�Ajr�AjI�Ai�PAi+Ah9XAfQ�Ac��AcXAcO�Ab��Ab-Aa`BA`-A^�9A^ �A]��A]"�A\�/A\�A[�FA[oAZ�AYƨAY+AX��AXjAXI�AV�DAU�mAUp�AS�PAR��AO��AL��AK�7AJ1AIhsAH��AFJAB�AA|�A@��A@~�A@5?A?��A>�\A=��A<�9A<(�A<1A;�#A;+A:�\A:$�A9��A9�A9S�A8�DA7��A7+A6��A6E�A5�wA4��A3�PA2��A2~�A2 �A1�A0�A0E�A/�A/ƨA/S�A.I�A-S�A,�yA,n�A+��A*�A)K�A(�A'�-A'x�A&�jA&JA%`BA#��A"��A"�A!hsA!G�A!+A!"�A �A $�A�A��Al�A��AI�A��AA�+AbA�FA&�A��Ap�A=qAJA�A�AjAQ�A$�A��A �A{A��AA=qAn�A;dA�!At�A
ȴA
A��A��A  A�7A��A�+A�A�yA  A �A bN@��@�\)@��\@���@�~�@���@��@�V@��y@�@���@���@���@���@�7L@��@�hs@�;d@�@�ƨ@�R@��@��@�bN@�A�@��@ޏ\@�ƨ@�p�@�&�@�1'@ם�@ׅ@ׅ@ו�@�K�@�n�@�9X@҇+@�O�@υ@��@�5?@��@ǶF@ư!@��@Ĭ@�j@�Q�@���@���@�V@��T@��-@�O�@��D@���@�+@���@��h@�  @�;d@��h@�t�@�Ĝ@��D@�9X@��@��@�&�@��`@��@�"�@���@���@��@��`@��P@�V@��^@�x�@��@��@��@��@���@�  @���@�t�@�C�@�@�^5@��@�O�@��`@�z�@� �@��w@���@�33@��@���@�=q@��^@�`B@��/@�z�@�r�@�r�@�j@�j@�Z@� �@��@���@��#@���@��@�Z@�;d@��h@���@���@���@�Q�@��@��@�"�@���@��\@�M�@�{@�?}@�1@�@���@�=q@��@�@��h@�p�@��@��@�j@�  @�t�@��H@��+@�5?@��T@���@�X@�&�@�Ĝ@�Z@�I�@�(�@�1@���@��@��P@�dZ@�+@���@��!@�n�@��@���@���@���@�O�@���@��u@�A�@��@�ƨ@���@��P@�t�@�S�@�33@�o@���@�n�@�{@���@��^@��-@��@�/@���@���@�z�@�j@�Q�@�I�@�I�@�9X@��@�P@~��@~ff@~{@}��@}`B@}O�@}O�@}�@|�j@|�D@|j@|I�@|Z@|j@|(�@{dZ@{"�@{"�@{"�@zn�@x�u@wl�@v��@v5?@u��@t��@tZ@tz�@t9X@t9X@tZ@st�@s"�@so@s@rn�@qx�@q%@pQ�@o��@o�@o�P@ol�@o+@o
=@n��@n�@n�R@n��@nv�@nff@nE�@nE�@n$�@m�@m@m�@m�-@m��@m`B@m/@m/@m?}@m/@mV@mV@l��@mV@l��@l��@l�@k�
@k33@j��@j^5@j=q@j=q@j�@i��@i�@h�`@h�`@h�9@h�u@hA�@g�;@g;d@f�@f�y@f�@f��@f��@fff@e�T@e@f@f@f@f@f@f@f{@f{@e�@e��@e@e�@d�@c�
@c33@b��@bJ@a��@a�@`�`@`�9@`��@`bN@`A�@`b@_��@_l�@^�y@^�+@_;d@_K�@_
=@^�y@^ȴ@^��@^v�@^V@^5?@^$�@]��@]`B@\��@\Z@[�F@[t�@Z�H@Y��@Y�@Y�#@Y��@Y��@YX@Yhs@YX@Y7L@Y&�@Y&�@X��@Xr�@X  @W�@W�@Wl�@V�y@V�@VE�@UV@Tj@S��@SS�@S@R~�@Q�#@Q��@QG�@P��@P �@O�@O��@O�P@OK�@N��@N@M�@M/@L�/@L�D@LI�@L(�@K�F@J�H@J��@Jn�@J-@I�#@I�7@I%@H�u@HbN@HA�@Hb@Gl�@F�R@F@E@E�-@E�@EO�@D�@Dj@C�F@C��@C�@Ct�@Ct�@Ct�@C�@CdZ@C33@C"�@B��@B=q@A�@A��@Ax�@AG�@A�@A%@A%@A%@@�`@@��@@Q�@@b@@  @?�w@?+@>ȴ@>��@>��@>ff@>$�@=�@=@=p�@=V@<�j@<�@<��@<��@<�D@<�D@<j@<(�@;�m@;t�@;o@:��@:�\@:~�@:M�@:�@9��@9�^@9X@9�@8��@8��@8�9@8�@8bN@8A�@8 �@8  @7�w@7�P@7l�@7;d@7
=@6�@6��@6�+@6V@6E�@6{@5�@5@5��@5p�@5V@4��@4��@4��@4�j@4�@4j@4�@3��@3"�@3o@3o@3@2��@2-@1��@1x�@1X@1G�@1�@0��@0bN@/�;@/�P@/\)@/;d@/�@.ȴ@.��@.ff@.5?@.@-�T@-��@-�-@-�h@-`B@-�@,�@,�D@,9X@,�@,1@+�
@+33@+"�@*�@*^5@)��@)x�@)G�@(��@(�9@(�u@(�u@(�@(bN@(A�@(  @'�P@'�@&��@&V@&@%��@%�-@%��@%�h@%�h@%?}@%�@$�@$��@$�@$�D@$j@$9X@#��@#ƨ@#dZ@#@"n�@"M�@"J@!��@!��@!��@!7L@ ��@ bN@ 1'@�@�w@�@��@K�@+@
=@�R@��@�h@O�@�@�/@�j@�j@��@j@�@��@C�@"�@�@n�@-@�7@��@Ĝ@�u@bN@�w@\)@K�@+@�@�y@ȴ@�R@�+@v�@5?@�-@p�@V@�/@�@z�@j@Z@I�@9X@ƨ@C�@�@��@�!@��@n�@M�@�@�@��@�@�@�^@��@%@��@��@�9@�9@��@�@1'@�@��@��@�w@�P@l�@\)@;d@
=@��@��@�y@�y@�@�@ȴ@�R@�R@�+@V@E�@5?@5?@$�@�@�T@@�-@��@��@�@p�@`B@`B@?}@�@V@��@�@�D@j@I�@I�@I�@9X@1@��@�@�@t�@S�@C�@
�H@
�\@
M�@
M�@
-@
�@	�@	�#@	�#@	�#@	��@	��@	X@	%@��@Ĝ@��@�@bN@A�@1'@ �@  @l�@+@
=@�y@�y@�y@�@ȴ@�R@�+@�+@ff@@��@@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B#�B#�B#�B#�B#�B#�B#�B$�B#�B$�B$�B$�B$�B%�B%�B$�B$�B%�B%�B&�B)�B33B=qBA�BK�BN�BR�BR�BR�BQ�BQ�BQ�BO�BH�B?}B-B+B�BB��B�-B��B�bB�Bw�Bn�B/B�B{BuBoBVBB�)B��B��BȴB�dB�RB�B��B��B��B��B��B��B�oB�BgmBH�B.B�BVB
��B
�B
�5B
�B
��B
�XB
��B
��B
�bB
�%B
{�B
n�B
aHB
Q�B
K�B
F�B
B�B
>wB
=qB
;dB
5?B
+B
%�B
#�B
!�B
�B
�B
hB
bB
VB
DB

=B
	7B
1B
B
  B	��B	�B	�)B	��B	��B	��B	��B	ƨB	�wB	�9B	�B	�B	��B	��B	��B	��B	��B	�{B	�\B	�=B	�+B	�B	�B	z�B	s�B	q�B	gmB	_;B	W
B	B�B	;dB	49B	/B	+B	%�B	VB	DB		7B	%B	B	B��B��B��B��B�B�B�B�B�B�B�B�B�yB�`B�TB�NB�BB�;B�#B�B��B��B��B��B��B��BɺBǮBƨBÖB�}B�qB�dB�RB�9B�'B�B��B��B��B��B��B��B��B��B�uB�oB�oB�hB�bB�\B�DB�+B�B�B�B~�B{�Bz�Bx�Bv�Bu�Br�Br�Bm�Bk�Bk�BhsBffBffBffBbNB_;B_;B^5B^5B[#BS�BP�BO�BK�BJ�BH�BF�BC�BC�B@�B@�B9XB8RB6FB49B2-B0!B/B.B-B,B)�B'�B%�B#�B"�B!�B �B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B{B{B�B�B�B�B�B�B�B�B�BuBoBuB{BuB�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B �B$�B#�B#�B#�B(�B.B33B49B49B5?B8RB9XB=qB=qBA�BB�BD�BD�BF�BF�BG�BG�BH�BM�BM�BM�BN�BN�BQ�BR�BT�BW
BXBXB[#B[#B^5B^5B_;BbNBdZBe`BjBo�Bs�Bs�Bt�Bt�Bt�Bv�Bz�B~�B�B�B�+B�DB�uB��B��B��B��B��B��B��B��B�B�B�B�B�-B�LB�qB��BBĜBŢBƨBǮBɺB��B��B��B�
B�B�)B�5B�BB�TB�`B�fB�B�B�B�B�B�B�B�B��B��B��B��B��B	  B	B	B	B	1B	PB	\B	hB	{B	�B	�B	�B	�B	�B	�B	�B	�B	"�B	%�B	(�B	(�B	)�B	+B	/B	33B	6FB	7LB	8RB	9XB	9XB	9XB	:^B	<jB	?}B	D�B	E�B	G�B	H�B	J�B	J�B	J�B	K�B	N�B	O�B	Q�B	R�B	T�B	VB	W
B	[#B	[#B	[#B	[#B	\)B	cTB	ffB	iyB	jB	k�B	n�B	s�B	t�B	t�B	t�B	v�B	y�B	z�B	{�B	|�B	�B	�+B	�1B	�JB	�JB	�PB	�PB	�PB	�\B	�\B	�bB	�oB	�oB	�oB	�uB	�uB	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�'B	�3B	�3B	�3B	�3B	�?B	�?B	�?B	�?B	�?B	�?B	�?B	�LB	�?B	�?B	�FB	�RB	�^B	�dB	�^B	�dB	�qB	��B	ŢB	ƨB	ƨB	ǮB	ǮB	ǮB	ȴB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�B	�B	�B	�#B	�#B	�#B	�/B	�NB	�TB	�`B	�`B	�fB	�mB	�mB	�sB	�mB	�sB	�yB	�B	�B	�B	�yB	�yB	�B	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
%B
%B
%B
%B
+B
1B
	7B
	7B
	7B

=B

=B
DB
JB
PB
VB
\B
\B
bB
bB
hB
oB
oB
uB
uB
uB
{B
�B
�B
�B
�B
�B
�B
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
"�B
"�B
"�B
"�B
"�B
"�B
"�B
#�B
#�B
#�B
$�B
$�B
%�B
%�B
%�B
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
+B
+B
+B
+B
+B
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
0!B
1'B
1'B
1'B
1'B
1'B
2-B
2-B
2-B
2-B
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
6FB
6FB
7LB
7LB
7LB
7LB
7LB
8RB
8RB
9XB
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
;dB
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
@�B
@�B
@�B
A�B
A�B
A�B
A�B
A�B
A�B
B�B
B�B
C�B
C�B
D�B
D�B
E�B
E�B
E�B
E�B
E�B
E�B
F�B
F�B
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
I�B
J�B
J�B
J�B
J�B
K�B
K�B
K�B
L�B
L�B
L�B
L�B
L�B
L�B
M�B
M�B
N�B
N�B
N�B
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
YB
YB
YB
YB
YB
YB
ZB
ZB
[#B
[#B
[#B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
]/B
]/B
]/B
^5B
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
e`B
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
iyB
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
n�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B#�B#�B#�B#�B#�B#�B#�B$�B#�B$�B$�B$�B$�B%�B%�B$�B$�B%�B%�B'8B*�B4B>(BB�BL�BO�BS�BS[BS�BS@BTBVBV9BPHBJ=B:*B�B�RB�:B��B��B��B�lB�B|PB2�B�B�B{BgB�B	�B�dB�gB��B��B��B��B��B��B�`B��B�!B��B��B�SB�zBk�BLJB0�BpB�B
�BB
��B
�VB
��B
��B
�6B
�NB
�QB
�oB
�fB
~�B
rGB
cnB
R�B
L�B
GzB
CGB
?B
>]B
=�B
6�B
+�B
&LB
$tB
# B
B
B
�B
�B
�B
xB

rB
	�B
	B
�B
�B	�VB	� B	ܬB	�FB	өB	��B	�B	�1B	�B	�B	��B	��B	��B	��B	��B	�~B	�eB	�gB	�HB	��B	��B	��B	��B	|B	t�B	tB	i_B	c B	ZB	D�B	="B	5tB	0�B	.�B	)�B	�B	�B		�B	�B	%B	gB�HB��B��B�B�9B�B�B�'B�B�!B�IB��B�B�B�B� B�HB�B�xB�B՛BԯB�BϫB�6B�DB�=BȴB�BĶB�4B�]B��B��B��B�-B�B��B��B��B�&B��B�!B�eB�?B��B��B��B�:B��B��B�~B��B��B�B�B�B|�B{�By�Bw�Bv�BtTBtBnBl�Bl�Bh�Bf�Bg8Bh
Bb�B_�B_�B_!B`\B]IBU�BQ�BQhBL�BK�BI�BG_BD�BD�BB�BB'B:xB9�B7�B5�B3B0�B/�B/B.�B-�B+kB)_B'mB%zB$tB"�B!HB 'B B vB B �B vBVBxBQBqBBB�B+B�B?B�B�B�B
B�B�B�B
ByBB�B�BB�B�B�B{BmBSBsB�B�BBkBCBBBCBIBdBBIB�B�B�B BB"hB&fB$ZB$tB%FB)�B./B3�B4�B4�B5�B8�B:*B>]B>wBB[BC-BEBEBGBG_BH�BH�BJ	BN<BNBN<BO(BOvBRoBS�BUgBWsBX_BXyB[WB[�B^jB^�B_�Bb�Bd�Be�Bj�Bo�Bs�Bs�Bt�Bt�Bu%BwfB{�B�B�{B��B��B�JB�{B��B��B��B�:B�ZB�fB�XB�DB�kB�WB��B��B�3B�B��B��B��B��B��B��B��B�=B�B�BB�[B�sBچBܒBބB�B�B�B��B��B��B��B��B��B��B��B��B�B�2B��B�0B�<B	 4B	AB	aB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	B	!B	#:B	&B	)*B	)*B	*0B	+QB	/�B	3�B	6`B	7�B	8lB	9rB	9�B	9rB	:�B	<�B	?�B	D�B	E�B	G�B	H�B	J�B	J�B	J�B	K�B	N�B	PB	R B	R�B	UB	V9B	WYB	[=B	[=B	[qB	[�B	\�B	c�B	f�B	i�B	j�B	k�B	n�B	s�B	t�B	t�B	t�B	wB	y�B	z�B	|B	}<B	�aB	�zB	��B	�~B	�~B	�jB	�jB	�jB	�vB	�vB	�}B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�2B	�_B	�cB	�vB	�hB	�MB	�hB	�MB	�tB	�tB	�ZB	�ZB	�ZB	�ZB	�tB	��B	�tB	�tB	�FB	�lB	��B	�dB	�xB	��B	��B	��B	żB	��B	ƨB	ǮB	��B	ǮB	ȴB	��B	��B	��B	�B	�B	�0B	�B	�B	�B	� B	�FB	�B	�9B	�$B	�$B	�1B	�1B	�QB	�WB	�qB	�=B	�B	�hB	�nB	�zB	�zB	�B	�B	�B	�B	�B	��B	��B	�B	��B	�B	�B	��B	�B	�B	�B	�B	�B	�B	�B	��B	��B	�B	��B	��B	�B	�B	�B	��B	�B	�0B	�B	�6B	�VB
 OB
;B
GB
SB
YB
YB
?B
YB
_B
fB
	lB
	lB
	RB

rB

�B
xB
~B
�B
�B
�B
vB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
#B
"�B
"�B
"�B
"�B
#B
#B
#�B
#�B
$B
%,B
$�B
%�B
&B
%�B
&B
'B
'B
'B
'8B
(
B
'�B
'�B
(
B
(�B
(�B
)B
)B
)B
)DB
*B
*0B
+B
+6B
+B
+B
+B
,=B
,WB
-CB
-)B
-)B
-CB
./B
./B
.IB
.IB
./B
./B
/OB
/5B
/OB
/OB
0;B
0;B
0;B
0;B
0;B
0UB
1AB
1[B
1AB
1[B
1[B
2aB
2-B
2-B
2aB
2GB
2aB
3MB
3hB
4nB
4TB
49B
4TB
4TB
5tB
6zB
6zB
7fB
7�B
7fB
7fB
7�B
8�B
8lB
9rB
9rB
9�B
9�B
:�B
:xB
:�B
;�B
;B
;B
;B
;B
<�B
<�B
<�B
<�B
=�B
=�B
=�B
=�B
>�B
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
A�B
A�B
B�B
B�B
C�B
C�B
D�B
D�B
E�B
E�B
E�B
E�B
E�B
E�B
F�B
F�B
F�B
F�B
F�B
F�B
G�B
G�B
G�B
G�B
IB
H�B
I�B
I�B
I�B
J�B
J�B
J�B
KB
K�B
K�B
K�B
MB
L�B
L�B
MB
L�B
L�B
N"B
N"B
OB
N�B
N�B
O�B
PB
O�B
PB
PB
P.B
QB
QB
R B
RB
R B
S&B
S&B
T,B
TB
TB
UB
U2B
V9B
VB
V9B
V9B
VB
VB
V9B
W$B
W?B
W$B
W?B
X+B
X+B
X+B
Y1B
YKB
YB
Y1B
Y1B
YKB
ZQB
ZQB
[=B
[WB
[=B
\)B
\]B
\]B
\]B
\CB
\]B
\)B
\CB
]IB
]IB
]dB
^OB
^OB
^OB
^OB
^jB
^OB
^OB
^jB
_;B
_VB
_VB
_VB
_VB
_;B
_pB
`\B
`BB
`\B
`BB
`BB
`\B
`BB
`BB
abB
aHB
abB
abB
bNB
bhB
bNB
bhB
c�B
cnB
cnB
cTB
cTB
cnB
cnB
dtB
dZB
dtB
e�B
e�B
e`B
e�B
ezB
e�B
ffB
f�B
f�B
f�B
f�B
f�B
g�B
g�B
gmB
gmB
g�B
g�B
h�B
h�B
i�B
i�B
i�B
i�B
i�B
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
n�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<,1<Q�<(�^<#�
<#�
<#�
<#�
<#�
<#�
<#�
<Y��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.08(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201610280038182016102800381820161028003818201806221215492018062212154920180622121549201804050408402018040504084020180405040840  JA  ARFMdecpA19c                                                                20161024093504  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20161024003524  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20161024003524  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20161024003524  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20161024003525  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20161024003525  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20161024003525  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20161024003525  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20161024003525  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20161024003526                      G�O�G�O�G�O�                JA  ARUP                                                                        20161024012317                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20161024153318  CV  JULD            G�O�G�O�F���                JM  ARCAJMQC2.0                                                                 20161027153818  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20161027153818  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404190840  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622031549  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115111517                      G�O�G�O�G�O�                