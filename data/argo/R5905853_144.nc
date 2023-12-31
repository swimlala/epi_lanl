CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2023-01-19T21:46:56Z creation;2023-01-19T21:47:01Z conversion to V3.1      
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
_FillValue                 �  I8   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M(   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tx   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �0   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �    PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �p   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �(   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ޘ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �    HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �D   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �T   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �X   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �h   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �l   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �p   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �tArgo profile    3.1 1.2 19500101000000  20230119214656  20230119215741  5905853                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA                                  2B  A   APEX                            8421                            2.11.2                          846 @�0.֩&1   @�0�ʆB@-m�hr�!�c!$�/1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @��@�  @�  A   A   A@  A`  A���A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B��B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx��B�  B�  B�  B�  B�  B�ffB���B�  B�  B�  B�33B�ffB���B���B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B���B�  B�  B�  B�  B�  B�  C   C  C  CL�C�fC
  C  C  C  C  C  C  C  C  C  C  C   C"  C$�C&33C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl�Cn�Cp�Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C��3C��3C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,�fD-fD-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY�fDZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�3D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�vf1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @z�@z�H@�p�@�p�A�RA>�RA^�RA�(�A�\)A�\)A�\)A�\)A�\)A�\)A�\)A�\)B�B�B�BG�B'�B/�B7�B?�BG�BO�BW�B_�Bg�Bo�Bxz�B�B��
B��
B��
B��
B�=pB���B��
B��
B��
B�
=B�=pB���B���B��
B��
B��
B��
B��
B��
B��
B��
Bף�B��
B��
B��B��
B��
B��
B��
B��
B��
B��
C�C�C8RC��C	�C�C�C�C�C�C�C�C�C�C�C�C!�C$C&�C'�C)�C+�C-�C/�C1�C3�C5�C7�C9�C;�C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�ClCnCpCq�Cs�Cu�Cw�Cy�C{�C}�C�C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D z�D ��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��D	z�D	��D
z�D
��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��D z�D ��D!z�D!��D"z�D"��D#z�D#��D$z�D$��D%z�D%��D&z�D&��D'z�D'��D(z�D(��D)z�D)��D*z�D*��D+z�D+��D,�GD-GD-z�D-��D.z�D.��D/z�D/��D0z�D0��D1z�D1��D2z�D2��D3z�D3��D4z�D4��D5z�D5��D6z�D6��D7z�D7��D8z�D8��D9z�D9��D:z�D:��D;z�D;��D<z�D<��D=z�D=��D>z�D>��D?z�D?��D@z�D@��DAz�DA��DBz�DB��DCz�DC��DDz�DD��DEz�DE��DFz�DF��DGz�DG��DHz�DH��DIz�DI��DJz�DJ��DKz�DK��DLz�DL��DMz�DM��DNz�DN��DOz�DO��DPz�DP��DQz�DQ��DRz�DR��DSz�DS��DTz�DT��DUz�DU��DVz�DV��DWz�DW��DXz�DX��DY�GDY��DZz�DZ��D[z�D[��D\z�D\��D]z�D]��D^z�D^��D_z�D_��D`z�D`��Daz�Da��Dbz�Db��Dcz�Dc��Ddz�Dd��Dez�De��Dfz�Df��Dgz�Dg��Dhz�Dh��Diz�Di��Djz�Dj��Dkz�Dk��Dlz�Dl��Dmz�Dm��Dnz�Dn��Doz�Do��Dpz�Dp��Dqz�Dq��Drz�Dr��Dsz�Ds��Dtz�Dt��Duz�Du��Dvz�Dv��Dwz�Dw��Dxz�Dx��Dyz�Dy��Dzz�Dz��D{z�D{��D|z�D|��D}z�D}��D~z�D~��Dz�D��D�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD� �D�=qD�}qD��qD��qD�=qD�}qD��qD� �D�@�D�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�z>D��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�@�D�}qD��qD��qD�=qD�}qD½qD��qD�=qD�}qDýqD��qD�=qD�}qDĽqD��qD�=qD�}qDŽqD��qD�=qD�}qDƽqD��qD�=qD�}qDǽqD��qD�=qD�}qDȽqD��qD�=qD�}qDɽqD��qD�=qD�}qDʽqD��qD�=qD�}qD˽qD��qD�=qD�}qD̽qD��qD�=qD�}qDͽqD��qD�=qD�}qDνqD��qD�=qD�}qDϽqD��qD�=qD�}qDнqD��qD�=qD�}qDѽqD��qD�=qD�}qDҽqD��qD�=qD�}qDӽqD��qD�=qD�}qDԽqD��qD�=qD�}qDսqD��qD�=qD�}qDֽqD��qD�=qD�}qD׽qD��qD�=qD�}qDؽqD��qD�=qD�}qDٽqD��qD�=qD�}qDڽqD��qD�=qD�}qD۽qD��qD�=qD�}qDܽqD��qD�=qD�}qDݽqD��qD�=qD�}qD޽qD��qD�=qD�}qD߽qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD��qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�s�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  AˬA˫�A˭�A˲�A˽<A˾�A˾�A˿A���A��RA��KA��?A���A��6A�͟A�ΥA�уA��TA��&A��,A��[A��2A��gA��A���A�ܒA��/A���A���A��;A��5A���A���A���A��NA��-A˘_A�iDA��A���A��8A��0A�.}A�(A��4A�ZQA��UA�9$A��2A��`A�1�A�y�A���A�RTA�XEA��A���A�3hA�(A��fA���A��A��=A�&LA��A�u�A��pA�!bA�/�A�kA�A�A��A�	A��.A�jA��A��ZA�33AAz"�Au \Ar�"Ap��Ao�AmW?Ak�Ag9�Ads�Ac-A`�fA^MAZ��AW��AT�qASA�AQ�AP�oAQ�AN{AJ��AE�}AD�AA$�A?B[A=�1A;��A8c�A5�"A4��A4P�A3iDA1�A0��A/�`A/~A-{A,��A+ɆA+F�A+RTA*	A(�"A'�A%֡A$^5A#>BA"kQA!��A!MA ��A!�A!:�A!_pA!͟A ��A aA .IA #:A 8�A ��A ��A ��A 5?AxA��Ab�A�A�A��AMA��A�}A�A�*AS�A�fA��A��AXyA�A;dAC�A;A��A��A��A�9A��A��A��A��Ah�A��AQ�A?A��Aw2A iA�AA��A2�ArGAGEA�AV�A
=A
�-A
��A
�LA
A	{�A	�A�OA�$A�$A2�A�SA�cA�|A��A�QA��A��AtTA��A�A�fA͟A�A�A��A��A]dA {JA �@�ݘ@�&�@��@�1'@�g8@�@�@�l�@���@�H�@�IR@�@��D@�:*@�q@�c @�n@���@���@��T@�8@�L@�~@��@�O@��@�_@��@�L�@��@�oi@�>�@��@��@��@��U@�4@�c @�2�@�s�@���@��@���@���@�>B@�g�@�҉@ތ�@�c�@�V@�-@�a@�\�@�X@��P@�~(@�&�@٤@@�o@��]@�(�@���@��m@��X@��?@�($@�@O@ԓu@��@���@�M�@�?}@�s�@�Q�@��@�B�@��@Ь�@�>B@ϿH@ς�@�#�@α�@�N�@ͼ@��	@̵�@̃�@�G@�s�@ʰ�@�J�@���@�X�@�s@Ǽ@ƯO@�6@ŵt@��K@�c@��6@��@��@�4@�D�@��>@��k@�{J@���@�X�@��p@��@�0U@���@��@��@��&@��6@��H@��2@�1�@��)@���@���@�V@��]@��@���@�8�@���@�c@�@���@�@���@��'@�4@��@��F@�l�@�6�@��+@�Vm@��H@��1@�_@��@�@��I@�/�@��@�X@��/@�[�@�@���@��@�IR@��}@�4n@� �@�˒@���@�q@���@���@�;d@��@��c@���@�)�@���@���@�[�@���@��@�Mj@���@���@�]d@�!@��a@�+@��R@�xl@�1'@��@���@��@��?@��@��A@�D�@�
�@���@�C�@���@���@��o@���@�J�@��w@�j@�\�@�[W@���@��@��W@���@��@��4@��@���@���@��"@�]�@��f@��@�s�@�V@�Ov@�@��^@�(�@��/@�Ɇ@���@�D�@���@���@�}�@�)_@��]@��<@��@��Z@�ԕ@��q@��f@�dZ@�E9@�,�@���@���@�H@��@�x@���@��S@�K�@�&@�;@��o@�@��m@���@�Y@�~�@�7�@���@�A�@�1�@���@��@��E@��m@�|�@��@��a@�v`@�[W@�4�@���@��v@��@��b@�\�@���@��	@�s�@�a@�J#@��@���@��j@��x@�i�@�	@��&@���@��z@��-@���@�2a@��@�l"@�Q@�(�@��@�o @��f@��e@�q@��@���@�zx@�n/@�Mj@���@��@���@�~(@�u%@�d�@�W�@�6�@� �@��@���@�E9@�@@��@���@��e@�H@˒@dZ@~��@~3�@}�C@}+�@|��@|��@|�@{��@{)_@{�@z�@z�'@y��@x�[@w��@w�@v5?@v@u�@uQ�@t�@s�A@s�	@s,�@r��@q�@qo @q<6@q!�@p�@p��@pD�@o�F@n�8@n��@n�@n\�@nV@n~�@n�\@nC�@n�@m��@l��@k�@j\�@j�@i8�@h�v@hl"@h	�@g�@@g�@f�1@f�@f�@f��@fh
@f@e��@es�@ee,@d�@d�$@d�I@d~(@d�o@dh�@c�}@b��@b��@bE�@b�@a�-@`h�@_9�@^�!@^V@^)�@]�d@]�=@][W@]G�@]J�@]+@\��@\>B@[�]@[�A@[�@[g�@[8@Z�H@Z��@Zz@ZE�@Z@Y�@YDg@Y#�@X�@Xoi@X�@W�6@W��@Wv`@W$t@V҉@V^5@U��@U;@T��@T��@T�@TA�@Tb@S��@S��@S8@R��@R��@RkQ@R-@Q��@Q�@Q7L@Q�@P�`@Pm�@O��@Oƨ@O��@OP�@O'�@O�@N��@N.�@M�3@M�=@Mc@M+@L��@LU2@L�@K�a@K)_@J��@I��@I/@HN�@Gخ@Gt�@G6z@F�]@F?@E�j@E�S@Eo @EO�@Eq@D�@D?�@C�@C�*@Cqv@C�@B��@Bxl@A��@AO�@A�@@��@@x@?��@?�@>��@>5?@=��@=5�@<֡@<�j@<�@<w�@<>B@<�@;j�@;�@:�@:�@:��@:~�@:^5@:C�@:3�@:e@9�>@9zx@9J�@98�@8�@8��@8I�@8G@7o�@76z@6�8@6ȴ@6�L@6:*@5��@5*0@4�E@4A�@4G@3��@3v`@3@2ff@1�Z@1S&@0�v@0��@0h�@0Xy@0C-@0b@/�&@/|�@/=@.��@.�@.�L@.�1@.^5@-�D@-�@-x�@-a�@-/@-	l@,��@,�`@,��@,��@,~(@,6@+��@+��@+x@+K�@+�@*�H@*q�@)�@)o @) \@)�@(�`@(��@(oi@(7@'� @'�@'�f@'a@'�@&�X@&�h@&��@&Ov@&!�@%��@%��@%�H@%��@%m]@%:�@$֡@$�O@$��@$`�@$"h@#��@#��@#~�@#g�@#9�@"�2@"p;@"3�@"�@!�o@!��@!��@!Y�@!Dg@ �@ ��@ Xy@�a@��@{J@]�@1�@o@�@�2@�}@s�@+k@�T@�@0�@�@U2@�@��@��@��@X�@C@�y@�s@�<@\�@($@�.@�)@�@�j@��@�@�M@O�@/@�	@�@m�@K^@%�@�@�g@�$@v`@qv@e�@S�@��@��@�@�@��@�1@��@c @�@�@��@w2@A @#�@@��@��@��@��@�D@��@C-@�@��@�@��@��@s@A�@.I@
=@��@҉@�@��@�x@c @$�@�@
�@��@��@B�@��@��@S�@�@��@�g@˒@��@��@�{@.I@S@�@{�@W�@H�@e@�@@<6@&�@�@��@��@�	@�f@�	@ѷ@�@S�@9X@��@��@n/@_p@J#@�@
��@
��@
�'@
� @
a|@
?@
�@	��@	ϫ@	k�@	�@�p@��@�.@q@bN@7�@�@�m@��@��@�{@]�@H�@H�@+@@�@�2@��@�1@a|@1�@@�o@�^1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  AˬA˫�A˭�A˲�A˽<A˾�A˾�A˿A���A��RA��KA��?A���A��6A�͟A�ΥA�уA��TA��&A��,A��[A��2A��gA��A���A�ܒA��/A���A���A��;A��5A���A���A���A��NA��-A˘_A�iDA��A���A��8A��0A�.}A�(A��4A�ZQA��UA�9$A��2A��`A�1�A�y�A���A�RTA�XEA��A���A�3hA�(A��fA���A��A��=A�&LA��A�u�A��pA�!bA�/�A�kA�A�A��A�	A��.A�jA��A��ZA�33AAz"�Au \Ar�"Ap��Ao�AmW?Ak�Ag9�Ads�Ac-A`�fA^MAZ��AW��AT�qASA�AQ�AP�oAQ�AN{AJ��AE�}AD�AA$�A?B[A=�1A;��A8c�A5�"A4��A4P�A3iDA1�A0��A/�`A/~A-{A,��A+ɆA+F�A+RTA*	A(�"A'�A%֡A$^5A#>BA"kQA!��A!MA ��A!�A!:�A!_pA!͟A ��A aA .IA #:A 8�A ��A ��A ��A 5?AxA��Ab�A�A�A��AMA��A�}A�A�*AS�A�fA��A��AXyA�A;dAC�A;A��A��A��A�9A��A��A��A��Ah�A��AQ�A?A��Aw2A iA�AA��A2�ArGAGEA�AV�A
=A
�-A
��A
�LA
A	{�A	�A�OA�$A�$A2�A�SA�cA�|A��A�QA��A��AtTA��A�A�fA͟A�A�A��A��A]dA {JA �@�ݘ@�&�@��@�1'@�g8@�@�@�l�@���@�H�@�IR@�@��D@�:*@�q@�c @�n@���@���@��T@�8@�L@�~@��@�O@��@�_@��@�L�@��@�oi@�>�@��@��@��@��U@�4@�c @�2�@�s�@���@��@���@���@�>B@�g�@�҉@ތ�@�c�@�V@�-@�a@�\�@�X@��P@�~(@�&�@٤@@�o@��]@�(�@���@��m@��X@��?@�($@�@O@ԓu@��@���@�M�@�?}@�s�@�Q�@��@�B�@��@Ь�@�>B@ϿH@ς�@�#�@α�@�N�@ͼ@��	@̵�@̃�@�G@�s�@ʰ�@�J�@���@�X�@�s@Ǽ@ƯO@�6@ŵt@��K@�c@��6@��@��@�4@�D�@��>@��k@�{J@���@�X�@��p@��@�0U@���@��@��@��&@��6@��H@��2@�1�@��)@���@���@�V@��]@��@���@�8�@���@�c@�@���@�@���@��'@�4@��@��F@�l�@�6�@��+@�Vm@��H@��1@�_@��@�@��I@�/�@��@�X@��/@�[�@�@���@��@�IR@��}@�4n@� �@�˒@���@�q@���@���@�;d@��@��c@���@�)�@���@���@�[�@���@��@�Mj@���@���@�]d@�!@��a@�+@��R@�xl@�1'@��@���@��@��?@��@��A@�D�@�
�@���@�C�@���@���@��o@���@�J�@��w@�j@�\�@�[W@���@��@��W@���@��@��4@��@���@���@��"@�]�@��f@��@�s�@�V@�Ov@�@��^@�(�@��/@�Ɇ@���@�D�@���@���@�}�@�)_@��]@��<@��@��Z@�ԕ@��q@��f@�dZ@�E9@�,�@���@���@�H@��@�x@���@��S@�K�@�&@�;@��o@�@��m@���@�Y@�~�@�7�@���@�A�@�1�@���@��@��E@��m@�|�@��@��a@�v`@�[W@�4�@���@��v@��@��b@�\�@���@��	@�s�@�a@�J#@��@���@��j@��x@�i�@�	@��&@���@��z@��-@���@�2a@��@�l"@�Q@�(�@��@�o @��f@��e@�q@��@���@�zx@�n/@�Mj@���@��@���@�~(@�u%@�d�@�W�@�6�@� �@��@���@�E9@�@@��@���@��e@�H@˒@dZ@~��@~3�@}�C@}+�@|��@|��@|�@{��@{)_@{�@z�@z�'@y��@x�[@w��@w�@v5?@v@u�@uQ�@t�@s�A@s�	@s,�@r��@q�@qo @q<6@q!�@p�@p��@pD�@o�F@n�8@n��@n�@n\�@nV@n~�@n�\@nC�@n�@m��@l��@k�@j\�@j�@i8�@h�v@hl"@h	�@g�@@g�@f�1@f�@f�@f��@fh
@f@e��@es�@ee,@d�@d�$@d�I@d~(@d�o@dh�@c�}@b��@b��@bE�@b�@a�-@`h�@_9�@^�!@^V@^)�@]�d@]�=@][W@]G�@]J�@]+@\��@\>B@[�]@[�A@[�@[g�@[8@Z�H@Z��@Zz@ZE�@Z@Y�@YDg@Y#�@X�@Xoi@X�@W�6@W��@Wv`@W$t@V҉@V^5@U��@U;@T��@T��@T�@TA�@Tb@S��@S��@S8@R��@R��@RkQ@R-@Q��@Q�@Q7L@Q�@P�`@Pm�@O��@Oƨ@O��@OP�@O'�@O�@N��@N.�@M�3@M�=@Mc@M+@L��@LU2@L�@K�a@K)_@J��@I��@I/@HN�@Gخ@Gt�@G6z@F�]@F?@E�j@E�S@Eo @EO�@Eq@D�@D?�@C�@C�*@Cqv@C�@B��@Bxl@A��@AO�@A�@@��@@x@?��@?�@>��@>5?@=��@=5�@<֡@<�j@<�@<w�@<>B@<�@;j�@;�@:�@:�@:��@:~�@:^5@:C�@:3�@:e@9�>@9zx@9J�@98�@8�@8��@8I�@8G@7o�@76z@6�8@6ȴ@6�L@6:*@5��@5*0@4�E@4A�@4G@3��@3v`@3@2ff@1�Z@1S&@0�v@0��@0h�@0Xy@0C-@0b@/�&@/|�@/=@.��@.�@.�L@.�1@.^5@-�D@-�@-x�@-a�@-/@-	l@,��@,�`@,��@,��@,~(@,6@+��@+��@+x@+K�@+�@*�H@*q�@)�@)o @) \@)�@(�`@(��@(oi@(7@'� @'�@'�f@'a@'�@&�X@&�h@&��@&Ov@&!�@%��@%��@%�H@%��@%m]@%:�@$֡@$�O@$��@$`�@$"h@#��@#��@#~�@#g�@#9�@"�2@"p;@"3�@"�@!�o@!��@!��@!Y�@!Dg@ �@ ��@ Xy@�a@��@{J@]�@1�@o@�@�2@�}@s�@+k@�T@�@0�@�@U2@�@��@��@��@X�@C@�y@�s@�<@\�@($@�.@�)@�@�j@��@�@�M@O�@/@�	@�@m�@K^@%�@�@�g@�$@v`@qv@e�@S�@��@��@�@�@��@�1@��@c @�@�@��@w2@A @#�@@��@��@��@��@�D@��@C-@�@��@�@��@��@s@A�@.I@
=@��@҉@�@��@�x@c @$�@�@
�@��@��@B�@��@��@S�@�@��@�g@˒@��@��@�{@.I@S@�@{�@W�@H�@e@�@@<6@&�@�@��@��@�	@�f@�	@ѷ@�@S�@9X@��@��@n/@_p@J#@�@
��@
��@
�'@
� @
a|@
?@
�@	��@	ϫ@	k�@	�@�p@��@�.@q@bN@7�@�@�m@��@��@�{@]�@H�@H�@+@@�@�2@��@�1@a|@1�@@�o@�^1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B	��B	��B	�DB	��B
B
 B
 B
 B
�B
gB
aB
oB
uB
MB
{B
�B
_B
	lB
	RB
	�B
EB
�B
EB

	B
�B
}B
�B
MB
�B
FB
�B
�B
�B
�B
_B
CB
!B
R:B
�WB
�,B
�_B
��B
��B
��B
u�B
��B
�dB
��B
�;B
�B
�B
��B
ðB
�%B
�GB
�zB
��B
�+B
��B
�AB
�B
~B
l=B
c:B
YeB
Q�B
K�B
L0B
I�B
AoB
3�B
(XB
VB
~B	�tB	��B	�B	��B	��B	�B	�sB	�B	�B	|B	rB	dZB	K�B	7�B	0oB	"NB	#B	�B	�B�xB�B�B	)B	# B	&LB	!�B��B�"B�jB�CB��BڠB�tB��B��B�-B�RB��B��B��B��B�PBބB�QBݘB�B�LB��B�^B�xB	fB	�B	�B	$@B	*B	7fB	KxB	aHB	sMB	��B	��B	�=B	��B	�&B	��B	�%B	�6B	ªB	�B	��B	�B	��B	�xB	ɆB	̈́B	�B	�6B	�gB	��B	��B	ʌB	��B	B	��B	��B	��B	��B	��B	��B	��B	��B	ƨB	ΊB	��B	�qB	�!B	��B	یB	�B	��B	ϑB	�B	��B	��B	��B	ϑB	��B	��B	�^B	��B	��B	�`B	�2B	��B	��B	��B	�<B	��B	��B	�*B	��B	�8B	��B	��B	��B	�PB	бB	�HB	�\B	�~B	�EB	ǮB	�KB	��B	��B	�.B	�4B	�dB	�0B	��B	żB	�fB	�^B	ŢB	��B	ĶB	�(B	�XB	��B	�xB	�B	�*B	�B	��B	��B	��B	��B	��B	�6B	�JB	�jB	�PB	��B	�<B	��B	�"B	��B	�}B	��B	��B	�B	��B	��B	�B	ÖB	ƨB	��B	ȚB	�1B	ɆB	˒B	�6B	�BB	ϑB	��B	��B	�B	��B	�mB	��B	ևB	�B	ּB	��B	�yB	�_B	��B	�sB	ѝB	�vB	�vB	�B	��B	�}B	҉B	��B	��B	��B	ԯB	��B	�B	ԯB	�hB	�
B	ڠB	�B	�B	�tB	�B	�@B	�tB	�`B	��B	�B	�B	�>B	�B	��B	��B	��B	�B	�DB	�2B	�B	�vB	�B	�B	�2B	�LB	�fB	��B	߾B	�4B	�B	�QB	�yB	�$B	�$B	�sB	�B	��B	�B	�B	�B	�IB	�IB	�B	��B	�B	�CB	�/B	�B	�5B	��B	�B	�B	�B	�'B	�aB	�GB	��B	�hB	�B	�B	��B	��B	�B	��B	��B	��B	�dB	�B	��B	��B	�PB	��B	��B	��B	�B	�HB	��B	�HB	�}B	��B
 iB
UB
�B
�B
AB
AB
-B
aB
aB
{B
{B
�B
B
B
SB
mB
�B
B
�B
�B
�B
�B
�B
_B
�B
1B
�B
�B
�B
�B
	�B
	lB
	B
	�B

#B

XB

rB

�B

�B
B
�B
B
�B
�B
B
B
�B
�B
�B
bB
�B
�B
}B
�B
�B
�B
�B
}B
}B
bB
HB
}B
bB
HB
HB
�B
�B
B
 B
�B
TB
 B
oB
[B
B
[B
�B
{B
B
�B
�B
�B
�B
�B
B
=B
]B
IB
�B
�B
�B
5B
pB
 BB
 BB
 �B
!bB
!�B
!�B
"NB
#B
#:B
#:B
#TB
"�B
"�B
# B
$B
%`B
%,B
%FB
%B
%zB
%`B
%�B
%�B
&LB
%�B
&B
&B
%�B
%�B
%�B
&�B
&�B
'�B
'�B
'�B
'�B
(>B
(�B
)yB
)�B
)�B
)�B
*�B
*�B
*�B
*�B
*�B
*�B
,B
,�B
-]B
-)B
-wB
-�B
-CB
-�B
-]B
-B
-B
-CB
-�B
-�B
-�B
-�B
.�B
.�B
/B
/ B
/�B
0�B
1B
0�B
1B
1AB
1vB
2-B
3hB
3�B
3�B
3�B
4B
4TB
49B
4TB
49B
4�B
4�B
6B
5�B
5�B
5?B
5ZB
5tB
5tB
5B
4�B
4TB
5B
4�B
4B
4B
4B
5?B
4�B
4�B
4TB
4TB
5%B
5B
5�B
5�B
5�B
5�B
5�B
6zB
72B
7LB
7fB
7�B
8�B
9�B
<B
<jB
<PB
<�B
<B
9�B
9	B
9	B
8�B
8�B
9XB
9�B
;0B
;�B
<6B
=�B
>wB
?HB
@OB
@�B
@�B
@�B
A B
@4B
?�B
@OB
A;B
BB
BuB
B�B
B�B
B�B
C-B
C{B
CaB
C�B
B�B
B�B
C-B
CGB
C�B
DMB
D�B
EB
F�B
GzB
HB
HKB
HfB
H�B
H1B
IB
H�B
IlB
IRB
IlB
IlB
IlB
IB
IRB
I�B
IlB
J=B
J�B
J�B
K^B
K)B
K�B
KxB
KxB
K�B
MPB
MB
MB
MjB
M�B
M�B
M�B
NB
M�B
MjB
MPB
M�B
N"B
NVB
N�B
O�B
P.B
P.B
P�B
P�B
P�B
QNB
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
RB
RB
R�B
S�B
SuB
S�B
TB
T,B
T�B
U2B
U�B
VB
V�B
W�B
WYB
WsB
W�B
XB
X�B
Y1B
YB
YB
YKB
YeB
ZB
ZB
ZB
ZkB
Z�B
ZQB
Z�B
[	B
[WB
[�B
[�B
[�B
[�B
[�B
\]B
\�B
]B
]�B
^B
^B
^5B
^jB
^�B
_!B
_;B
_;B
_pB
_VB
_pB
_�B
_�B
_�B
`B
`BB
`�B
`�B
`�B
`�B
`�B
`�B
a|B
a�B
a�B
a�B
a�B
a�B
b4B
bhB
b�B
b�B
c:B
cnB
c�B
dZB
dtB
eB
d�B
e�B
e�B
e�B
ffB
ffB
f�B
gB
gB
g�B
h
B
h�B
h�B
h�B
h�B
h�B
i�B
i�B
j0B
jKB
j�B
j�B
j�B
j�B
k6B
j�B
kB
kB
k�B
k�B
k�B
k�B
l=B
l=B
lWB
l�B
l�B
l�B
m)B
m]B
m]B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
nB
nB
nIB
nIB
n�B
n�B
n�B
n�B
n�B
n�B
oB
o�B
o�B
o�B
o�B
o�B
p!B
pUB
p�B
p�B
p�B
qB
q�B
q�B
q�B
q�B
rB
rB
r|B
raB
r�B
r�B
sB
s�B
s�B
s�B
s�B
tB
t9B
tTB
tnB
t�B
t�B
u%B
utB
utB
vB
v+B
v�B
w2B
wB
w2B
wLB
w�B
w�B
xB
w�B
xB
xlB
x�B
x�B
x�B
x�B
x�B
x�B
x�B
y>B
y�B
y�B
y�B
z*B
zDB
z^B
z�B
z�B
{B
{0B
{dB
{JB
{0B
{JB
{�B
{�B
{�B
{�B
|B
|6B
|B
|6B
|�B
|�B
}VB
}�B
}�B
}�B
}�B
~B
~B
~]B
~�B
~]B
~]B
~�B
~�B
~�B
~�B
B
B
HB
}B
�B
�B
�B
�B
�B
� B
�B
��B
��B
��B
��B
�;B
�UB
��B
��B
�'B
�AB
��B
��B
��B
��B
��B
��B
��B
�B
�-B
�GB
��B
��B
��B
��B
��B
��B
�SB
�mB
�mB
��B
��B
��B
��B
��B
��B
�?B
��B
��B
��B
�_B
�zB
�zB
��B
��B
��B
�B
�B
�KB
��B
��B
��B
��B
��B
�RB
�7B
��B
��B
��B
��B
��B
�#B
�=B
��B
��B
��B
��B
�B
�)B
�)B
�)B
�^B
�DB
�xB
��B
��B
�B
�dB
�JB
�~B
��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B	��B	��B	�DB	��B
B
 B
 B
 B
�B
gB
aB
oB
uB
MB
{B
�B
_B
	lB
	RB
	�B
EB
�B
EB

	B
�B
}B
�B
MB
�B
FB
�B
�B
�B
�B
_B
CB
!B
R:B
�WB
�,B
�_B
��B
��B
��B
u�B
��B
�dB
��B
�;B
�B
�B
��B
ðB
�%B
�GB
�zB
��B
�+B
��B
�AB
�B
~B
l=B
c:B
YeB
Q�B
K�B
L0B
I�B
AoB
3�B
(XB
VB
~B	�tB	��B	�B	��B	��B	�B	�sB	�B	�B	|B	rB	dZB	K�B	7�B	0oB	"NB	#B	�B	�B�xB�B�B	)B	# B	&LB	!�B��B�"B�jB�CB��BڠB�tB��B��B�-B�RB��B��B��B��B�PBބB�QBݘB�B�LB��B�^B�xB	fB	�B	�B	$@B	*B	7fB	KxB	aHB	sMB	��B	��B	�=B	��B	�&B	��B	�%B	�6B	ªB	�B	��B	�B	��B	�xB	ɆB	̈́B	�B	�6B	�gB	��B	��B	ʌB	��B	B	��B	��B	��B	��B	��B	��B	��B	��B	ƨB	ΊB	��B	�qB	�!B	��B	یB	�B	��B	ϑB	�B	��B	��B	��B	ϑB	��B	��B	�^B	��B	��B	�`B	�2B	��B	��B	��B	�<B	��B	��B	�*B	��B	�8B	��B	��B	��B	�PB	бB	�HB	�\B	�~B	�EB	ǮB	�KB	��B	��B	�.B	�4B	�dB	�0B	��B	żB	�fB	�^B	ŢB	��B	ĶB	�(B	�XB	��B	�xB	�B	�*B	�B	��B	��B	��B	��B	��B	�6B	�JB	�jB	�PB	��B	�<B	��B	�"B	��B	�}B	��B	��B	�B	��B	��B	�B	ÖB	ƨB	��B	ȚB	�1B	ɆB	˒B	�6B	�BB	ϑB	��B	��B	�B	��B	�mB	��B	ևB	�B	ּB	��B	�yB	�_B	��B	�sB	ѝB	�vB	�vB	�B	��B	�}B	҉B	��B	��B	��B	ԯB	��B	�B	ԯB	�hB	�
B	ڠB	�B	�B	�tB	�B	�@B	�tB	�`B	��B	�B	�B	�>B	�B	��B	��B	��B	�B	�DB	�2B	�B	�vB	�B	�B	�2B	�LB	�fB	��B	߾B	�4B	�B	�QB	�yB	�$B	�$B	�sB	�B	��B	�B	�B	�B	�IB	�IB	�B	��B	�B	�CB	�/B	�B	�5B	��B	�B	�B	�B	�'B	�aB	�GB	��B	�hB	�B	�B	��B	��B	�B	��B	��B	��B	�dB	�B	��B	��B	�PB	��B	��B	��B	�B	�HB	��B	�HB	�}B	��B
 iB
UB
�B
�B
AB
AB
-B
aB
aB
{B
{B
�B
B
B
SB
mB
�B
B
�B
�B
�B
�B
�B
_B
�B
1B
�B
�B
�B
�B
	�B
	lB
	B
	�B

#B

XB

rB

�B

�B
B
�B
B
�B
�B
B
B
�B
�B
�B
bB
�B
�B
}B
�B
�B
�B
�B
}B
}B
bB
HB
}B
bB
HB
HB
�B
�B
B
 B
�B
TB
 B
oB
[B
B
[B
�B
{B
B
�B
�B
�B
�B
�B
B
=B
]B
IB
�B
�B
�B
5B
pB
 BB
 BB
 �B
!bB
!�B
!�B
"NB
#B
#:B
#:B
#TB
"�B
"�B
# B
$B
%`B
%,B
%FB
%B
%zB
%`B
%�B
%�B
&LB
%�B
&B
&B
%�B
%�B
%�B
&�B
&�B
'�B
'�B
'�B
'�B
(>B
(�B
)yB
)�B
)�B
)�B
*�B
*�B
*�B
*�B
*�B
*�B
,B
,�B
-]B
-)B
-wB
-�B
-CB
-�B
-]B
-B
-B
-CB
-�B
-�B
-�B
-�B
.�B
.�B
/B
/ B
/�B
0�B
1B
0�B
1B
1AB
1vB
2-B
3hB
3�B
3�B
3�B
4B
4TB
49B
4TB
49B
4�B
4�B
6B
5�B
5�B
5?B
5ZB
5tB
5tB
5B
4�B
4TB
5B
4�B
4B
4B
4B
5?B
4�B
4�B
4TB
4TB
5%B
5B
5�B
5�B
5�B
5�B
5�B
6zB
72B
7LB
7fB
7�B
8�B
9�B
<B
<jB
<PB
<�B
<B
9�B
9	B
9	B
8�B
8�B
9XB
9�B
;0B
;�B
<6B
=�B
>wB
?HB
@OB
@�B
@�B
@�B
A B
@4B
?�B
@OB
A;B
BB
BuB
B�B
B�B
B�B
C-B
C{B
CaB
C�B
B�B
B�B
C-B
CGB
C�B
DMB
D�B
EB
F�B
GzB
HB
HKB
HfB
H�B
H1B
IB
H�B
IlB
IRB
IlB
IlB
IlB
IB
IRB
I�B
IlB
J=B
J�B
J�B
K^B
K)B
K�B
KxB
KxB
K�B
MPB
MB
MB
MjB
M�B
M�B
M�B
NB
M�B
MjB
MPB
M�B
N"B
NVB
N�B
O�B
P.B
P.B
P�B
P�B
P�B
QNB
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
RB
RB
R�B
S�B
SuB
S�B
TB
T,B
T�B
U2B
U�B
VB
V�B
W�B
WYB
WsB
W�B
XB
X�B
Y1B
YB
YB
YKB
YeB
ZB
ZB
ZB
ZkB
Z�B
ZQB
Z�B
[	B
[WB
[�B
[�B
[�B
[�B
[�B
\]B
\�B
]B
]�B
^B
^B
^5B
^jB
^�B
_!B
_;B
_;B
_pB
_VB
_pB
_�B
_�B
_�B
`B
`BB
`�B
`�B
`�B
`�B
`�B
`�B
a|B
a�B
a�B
a�B
a�B
a�B
b4B
bhB
b�B
b�B
c:B
cnB
c�B
dZB
dtB
eB
d�B
e�B
e�B
e�B
ffB
ffB
f�B
gB
gB
g�B
h
B
h�B
h�B
h�B
h�B
h�B
i�B
i�B
j0B
jKB
j�B
j�B
j�B
j�B
k6B
j�B
kB
kB
k�B
k�B
k�B
k�B
l=B
l=B
lWB
l�B
l�B
l�B
m)B
m]B
m]B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
nB
nB
nIB
nIB
n�B
n�B
n�B
n�B
n�B
n�B
oB
o�B
o�B
o�B
o�B
o�B
p!B
pUB
p�B
p�B
p�B
qB
q�B
q�B
q�B
q�B
rB
rB
r|B
raB
r�B
r�B
sB
s�B
s�B
s�B
s�B
tB
t9B
tTB
tnB
t�B
t�B
u%B
utB
utB
vB
v+B
v�B
w2B
wB
w2B
wLB
w�B
w�B
xB
w�B
xB
xlB
x�B
x�B
x�B
x�B
x�B
x�B
x�B
y>B
y�B
y�B
y�B
z*B
zDB
z^B
z�B
z�B
{B
{0B
{dB
{JB
{0B
{JB
{�B
{�B
{�B
{�B
|B
|6B
|B
|6B
|�B
|�B
}VB
}�B
}�B
}�B
}�B
~B
~B
~]B
~�B
~]B
~]B
~�B
~�B
~�B
~�B
B
B
HB
}B
�B
�B
�B
�B
�B
� B
�B
��B
��B
��B
��B
�;B
�UB
��B
��B
�'B
�AB
��B
��B
��B
��B
��B
��B
��B
�B
�-B
�GB
��B
��B
��B
��B
��B
��B
�SB
�mB
�mB
��B
��B
��B
��B
��B
��B
�?B
��B
��B
��B
�_B
�zB
�zB
��B
��B
��B
�B
�B
�KB
��B
��B
��B
��B
��B
�RB
�7B
��B
��B
��B
��B
��B
�#B
�=B
��B
��B
��B
��B
�B
�)B
�)B
�)B
�^B
�DB
�xB
��B
��B
�B
�dB
�JB
�~B
��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20230119035433  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8b                                                                20230119214656  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20230119214700  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20230119214701                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20230119214702  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20230119214702  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20230119215741                      G�O�G�O�G�O�                