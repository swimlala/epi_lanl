CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-08-16T09:49:06Z creation;2022-08-16T09:49:07Z conversion to V3.1      
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
_FillValue                    �PArgo profile    3.1 1.2 19500101000000  20220816094906  20220816095931  4902988                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               hA   JA                                  2B  A   APEX                            8600                            2.11.3                          846 @��P�� 1   @��P�io@:���S���c�vȴ9X1   GPS     A   A   F   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @���@�  A   A   A@  A`  A�  A�  A���A�  A�  A�  A�33A�33B   B  B  B  B   B(  B0  B8  B@  BH  BPffBX  B_��Bh  Bp  Bx  B��B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�33B�  B�  B�  B�  B�  B���C  C  C  C�C
�C  C�fC�fC�fC  C  C  C�fC  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP�CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj�Cl  Cn  Cp  Cr  Ct  Cv  Cx�Cz  C|  C~  C�fC��C��C��3C�  C�  C�  C�  C�  C��C�  C�  C��C��C��C��C�  C�  C�  C�  C�  C��C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C��3C��3C�  C�  C��C��C�  C�  C��C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  D   D � D ��D� D  D� D  D� D��D� D  D� D  Dy�D��D� D  D� D	  D	� D
  D
y�D  D� D  Dy�D  D� D  Dy�D��D� D  D� D  D� DfD� D  D� D  Dy�D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!fD!� D"  D"� D#  D#� D$  D$�fD%  D%� D&  D&� D'  D'� D(  D(� D)  D)y�D)��D*y�D+  D+� D,fD,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5y�D6  D6� D7  D7�fD8  D8y�D9  D9� D:  D:� D;  D;� D<  D<y�D=  D=y�D=��D>� D?fD?� D@  D@� DA  DA� DA��DB� DC  DC� DD  DD� DE  DEy�DE��DFy�DF��DGy�DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP�fDQ  DQ� DR  DR� DS  DS� DS��DT� DU  DU�fDV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D_��D`y�Da  Da� Db  Dby�Dc  Dc� Dd  Dd� Dd��Dey�Df  Df� Dg  Dg� DhfDh�fDi  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� DofDo� Dp  Dp�fDq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx�fDyfDy� Dz  Dz� D{  D{�fD|  D|� D}  D}� D~  D~� D  Dy�D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�C3D�� D�� D���D�@ D�� D�� D���D�@ D�� D�� D�  D�<�D�|�D���D�  D�@ D�� D�� D�  D�<�D�|�D���D�  D�C3D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D��3D�  D�@ D�|�D���D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÃ3D�� D�  D�@ DĀ D�� D���D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�<�Dǀ D��3D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�<�D�|�D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D���D�<�Dр D�� D�3D�@ DҀ DҼ�D���D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�3D�C3Dր D�� D�  D�<�D׀ D�� D�3D�@ D؀ Dؼ�D�  D�@ D�|�D�� D�  D�<�D�|�Dڼ�D�  D�@ Dۀ D�� D�3D�C3D܀ D�� D�  D�<�D݀ D�� D�  D�C3Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�<�D� D�� D�  D�@ D� D��D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�3D��3D�  D�@ D� D�� D�  D�@ D� D�� D�  D�C3D� D�� D�  D�@ D�3D�� D�  D�@ D� D�� D�  D�<�D� D�� D�  D�@ D� D��D�  D�@ D� D�� D�  D�<�D�|�D�� D�  D�@ D� D�� D�  D�C3D��3D�� D�  D�@ D� D�� D�  D�<�D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�3D�C3D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�ff111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@�Q�A (�A (�A@(�A`(�A�{A�{A��HA�{A�{A�{A�G�A�G�B 
=B
=B
=B
=B 
=B(
=B0
=B8
=B@
=BH
=BPp�BX
=B_��Bh
=Bp
=Bx
=B��B�B�B�B�B�B�B�B�B�8RB�B�B�B�B�B�B�B�B�B�B�B�B���B�B�B�B�8RB�B�B�B�B�B���C�C�C�C)C
)C�C��C��C��C�C�C�C��C�C�C �C"�C$�C&�C(�C*�C,�C.�C0�C2�C4�C6�C8�C:�C<�C>�C@�CB�CD�CF�CH�CJ�CL�CN�CP)CR�CT�CV�CX�CZ�C\�C^�C`�Cb�Cd�Cf�Ch�Cj)Cl�Cn�Cp�Cr�Ct�Cv�Cx)Cz�C|�C~�C��C�C�C��{C�HC�HC�HC�HC�HC�C�HC�HC�C�C�C�C�HC�HC�HC�HC�HC�C�HC�HC�C�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC��{C�HC�HC�HC�HC�HC�HC�C�C�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�C�HC��{C��{C�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC��{C��{C��{C��{C�HC�HC�C�C�HC�HC�C�HC�HC�HC�HC�HC�HC��{C�HC�HC�HC�HC�C�C�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC��{C�HC�HC�HC�HC�HC�HC�HC�HD  �D ��D �>D��D �D��D �D��D�>D��D �D��D �Dz>D�>D��D �D��D	 �D	��D
 �D
z>D �D��D �Dz>D �D��D �Dz>D�>D��D �D��D �D��D
D��D �D��D �Dz>D �D�
D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D  �D ��D!
D!��D" �D"��D# �D#��D$ �D$�
D% �D%��D& �D&��D' �D'��D( �D(��D) �D)z>D)�>D*z>D+ �D+��D,
D,��D- �D-��D. �D.��D/ �D/��D0 �D0��D1 �D1��D2 �D2��D3 �D3��D4 �D4��D5 �D5z>D6 �D6��D7 �D7�
D8 �D8z>D9 �D9��D: �D:��D; �D;��D< �D<z>D= �D=z>D=�>D>��D?
D?��D@ �D@��DA �DA��DA�>DB��DC �DC��DD �DD��DE �DEz>DE�>DFz>DF�>DGz>DH �DH��DI �DI��DJ �DJ��DK �DK��DL �DL��DM �DM��DN �DN��DO �DO��DP �DP�
DQ �DQ��DR �DR��DS �DS��DS�>DT��DU �DU�
DV �DV��DW �DW��DX �DX��DY �DY��DZ �DZ��D[ �D[��D\ �D\��D] �D]��D^ �D^��D_ �D_��D_�>D`z>Da �Da��Db �Dbz>Dc �Dc��Dd �Dd��Dd�>Dez>Df �Df��Dg �Dg��Dh
Dh�
Di �Di��Dj �Dj��Dk �Dk��Dl �Dl��Dm �Dm��Dn �Dn��Do
Do��Dp �Dp�
Dq �Dq��Dr �Dr��Ds �Ds��Dt �Dt��Du �Du��Dv �Dv��Dw �Dw��Dx �Dx�
Dy
Dy��Dz �Dz��D{ �D{�
D| �D|��D} �D}��D~ �D~��D �Dz>D� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD��D�C�D��RD��RD��D�@RD��RD��RD��D�@RD��RD��RD� RD�=D�}D��D� RD�@RD��RD��RD� RD�=D�}D��D� RD�C�D���D��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��D� RD�C�D��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�C�D��RD��RD� RD�@RD��RD�ÅD� RD�@RD�}D��D� RD�C�D��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�C�D��RD��RD� RD�@RD��RD��RD� RD�@RD�}D��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD�}D��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�C�D��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD���D��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD�}D��RD� RD�@RD��RD��RD� RD�C�D��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RDRD��RD� RD�@RDÃ�D��RD� RD�@RDĀRD��RD��D�@RDŀRD��RD� RD�@RDƀRD��RD� RD�=DǀRD�ÅD� RD�@RDȀRD��RD� RD�@RDɀRD��RD� RD�@RDʀRD��RD� RD�=D�}D��RD� RD�@RD̀RD��RD� RD�@RD̀RD��RD� RD�@RD΀RD��RD� RD�@RDπRD��RD� RD�@RDЀRD��RD��D�=DрRD��RD��D�@RDҀRDҽD��D�@RDӀRD��RD� RD�@RDԀRD��RD� RD�@RDՀRD��RD��D�C�DրRD��RD� RD�=D׀RD��RD��D�@RD؀RDؽD� RD�@RD�}D��RD� RD�=D�}DڽD� RD�@RDۀRD��RD��D�C�D܀RD��RD� RD�=D݀RD��RD� RD�C�DހRD��RD� RD�@RD߀RD��RD� RD�@RD��RD��RD� RD�=D�RD��RD� RD�@RD�RD�D� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD僅D�ÅD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�C�D�RD��RD� RD�@RD郅D��RD� RD�@RD�RD��RD� RD�=D�RD��RD� RD�@RD�RD�D� RD�@RD�RD��RD� RD�=D�}D��RD� RD�@RD�RD��RD� RD�C�D���D��RD� RD�@RD�RD��RD� RD�=D�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD��RD�ÅD� RD�@RD��RD��RD��D�C�D��RD�ÅD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD�f�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A���A�6�AӖA�S�A�@�A�A�A�r�A�e`A�C�A�8RA�0UA�
	A��[A�ĜAүOAүAҨ�Aҟ�A҇_A�K^A�� A��]A�V9A�S�A���A��GA�7�A��A�"�A�aHA��vA�ԕA�($A�-�A�PHA��KA�aA�aA���A��^A�Q�A�V�A��A�E9A�zxA�kA��A�bA��XA��aA��PA���A���A��tA�NpA�g8A��A���A�3hA���A���A���A���A��"A���A��'A��aA�kA�t�A�	�A�� A�~�A�s�A��A�X�A�1�A��FA�}"A���A�aA���A��-A�%FA�HA�1�A�,=A�2�A��sA�� A�%A���A�k�A�A��A��A��~A�XEA�
�A�K�A?�A|ѷAu�<As�HAq|�Ao��An��An1'An*�Am�.Am6Ai�+Ag�"AgS�Ag�Af��Af2aAd�oAdP�Ad/�AdSAb��Aa7�A`4A_��A^�A]�vAX��ATیASo AQ�SAP=qAOe�AN �AL�AL�AJ�*AH�]AG��AG2�AF�hAF/�AE��AD�pAC�gAB��AA�uA@��A?�A=�A;��A;�wA;�LA;CA9�$A8��A7ffA6�A4��A2�A1�
A1��A1]�A1%A0a|A/��A.��A,�;A+0�A*A*��A)��A(�A(_pA'�MA&͟A%�XA$=qA#�{A#1�A"��A"�VA"�A� AZA�AK^A(�AL�A0�AQ�A/�A�BA \A�A�XA!�AE�A�A�sA��A��A�A
��A
��A
�A	zA	YKA	A�A�KA�dATaA9�A��A|�AoAC�A��A�	A�oA ��A 	@��@�xl@��^@�j�@�e@��@��+@���@��@�e@��t@�n/@�7L@��z@蠐@�zx@柾@��,@�{@�M�@߮�@ߚk@߀4@��P@��@�_p@ܽ<@�bN@�/�@��>@�c�@ڹ$@�!-@ע�@ֹ�@�1@ՍP@��@�7@��@�<�@�x�@�+@�z@���@�Vm@��@˅@��@ʕ@ɪ�@�;d@Ȋr@Ǡ'@�E�@�'�@Ę_@�k�@�1�@�A @�-w@�w�@�w2@���@�l�@�{J@���@���@�\�@���@��Z@���@�<�@���@��9@�@���@�C�@�qv@��
@��Z@��@���@�9X@���@�@�7@��@��
@��K@���@��@��~@��{@�A�@��@�6�@��@���@�a|@�>B@�/�@�C�@�
�@�|@�"h@�@��@�O�@�+@���@�s@��,@��b@��@��'@�l"@�_@��r@��@�خ@�H�@��F@��@�� @��0@�x@���@��O@�L0@���@�^�@�/�@��@���@���@���@�~(@���@���@�J#@��+@���@���@��a@��@��*@���@�}�@��@���@��x@�[�@���@�dZ@�@O@�J@�bN@�%@���@���@��+@�@�@��@��H@�C�@��U@��@�`�@�I�@�~(@��p@��@�X@�(�@���@�]d@��@��)@��@���@�l�@�4@��@��?@��b@���@�w�@�r�@�l�@�PH@��Z@��h@�F@��@���@��L@�p;@�h�@�J�@�%�@��@��
@��d@���@�f�@��@��@���@��s@��@�s�@�1@���@�F@���@��v@���@�ȴ@��9@�y>@�a|@�-�@��g@���@���@�=@���@��@�oi@��@���@�e�@�+@��I@�7@�@�]@��@~Q@|�p@|l"@|��@|��@|?�@|  @{/�@z^5@zR�@y��@xQ�@vl�@v6�@v@u��@tbN@s��@s�@q�~@p:�@p�@o�+@o��@n��@n$�@m�o@m�N@m��@mDg@l��@l'R@k�@k��@k�[@k˒@k�q@k��@l�@l$@lN�@lN�@l[�@k��@kdZ@j�@j�L@jR�@i�Z@i��@i�T@i��@i�~@h�@h��@h�@h�O@h��@h$@g��@g@O@f�h@fh
@f�@e�@e�@e��@eu�@d�)@dG@c��@cH�@b��@b��@b��@b��@bh
@bu@a�S@a(�@a�@`�_@`u�@`Q�@`$@_�V@_Mj@_]�@_b�@^�X@^
�@]��@\�p@\V�@\@[�6@[�K@[�$@Z��@Z��@Z�@Y�=@Y(�@X�v@X�@Wb�@W!-@W$t@W�@V҉@V�x@V��@V�x@V��@V��@VM�@V8�@V.�@V�@V�@V
�@U�@T��@TS�@S��@Sy�@S i@R�B@R��@R��@R	@R �@Q��@Q�S@P�j@P2�@Pb@O�@O��@O��@OA�@O1�@O&@N��@Nc @N$�@MG�@L��@L�@K��@K��@K��@K�@Kn/@K)_@J�@J�8@J��@J��@J)�@I8�@HQ�@G��@G�@@GF�@G�@F�"@F�,@F�<@F��@F�F@F~�@Fi�@F^5@FM�@F0U@F�@Fe@Fe@F@E�D@E�n@E��@E��@EIR@E�@D�@D�j@D�@Dq@D9X@C�V@C@B҉@B��@BJ�@B0U@B�@B_@A�@A��@Ak�@ADg@@�@@q@@�@?Z�@>�@>��@>i�@>:*@>.�@>�@>O@>4@>�@=�@=��@=�@=��@<��@;��@;J#@:��@9��@9�H@9x�@9A @9q@8�@8��@7�&@7�P@7�@6�@6��@6�x@6�A@6a|@6+k@6�@5��@5�@5�@5@5��@5IR@4�4@4�@4N�@3�w@3�	@3y�@3iD@3Z�@3S�@3/�@2Q@2{@2�@1�@1��@1(�@0�f@0�u@0 �@/�a@/�k@/�@.��@.�@.��@.s�@.d�@.&�@-�D@-�@-ϫ@-�'@-��@-��@-��@-��@-��@-�"@-hs@-%@,V�@,  @+ݘ@+ݘ@+ݘ@+��@+��@+H�@*�R@*��@*~�@*z@*{�@*p;@*p;@*i�@*V@*:*@*#:@)�.@)@)��@)�h@)w2@)O�@([�@'��@'g�@'�@&��@&�R@&��@&��@&��@&��@&}V@&a|@&;�@&�@%�>@%�@%c@%k�@%L�@$V�@#�@#,�@"��@"ȴ@"��@"c @!�.@!�z@!V@ ��@ �U@ N�@�*@�2@@@�@]d@��@� @dZ@�@�@�6@�F@�r@s�@p;@Z�@B[@0U@_@�N@��@��@p�@f�@`B@Vm@=�@ \@�I@�@@9�@/�@
=@{@�9@��@p�@B�@�[@	�@�@��@�6@�0@�P@s@X�@6z@
=@��@�@�@�M@�@�c@�c@��@��@�]@�]@�@�s@��@��@�+@V@$�@
�@�3@�?@N�@I�@?�@-�@%�@"h@�r@�&@�@�
@��@�@��@�$@��@b�@P�@6z@&@.I@1�@;d@C�@@O@,�@��@u%@i�@Ov@GE@;�@($@&�@�C@�@��@��@j@@q@�@�|@�$@~(@_@9X@7@g�@�@
��@
ȴ@
�@
�R@
��@
��@
��@
��@
�@
��@
}V@
v�@
Z�@
R�@
;�@
-@
#:@
@	��@	�@	��@	�N@	�@	�@	ϫ@	�@	��@	�t@	�@	��@	p�@	A @	�@ѷ@S�@(�@!@	�@G@��@dZ@��@�@^5@3�@!�@#:@��@�@�^@��@k�@[W@^�@N<@<6@(�@#�@#�@�@�K@�)@��@Q�@�g@]�@�@�@��@�]@ߤ@ں@�H@�@��@#:@m]@ ��@ bN?���?�H�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A���A�6�AӖA�S�A�@�A�A�A�r�A�e`A�C�A�8RA�0UA�
	A��[A�ĜAүOAүAҨ�Aҟ�A҇_A�K^A�� A��]A�V9A�S�A���A��GA�7�A��A�"�A�aHA��vA�ԕA�($A�-�A�PHA��KA�aA�aA���A��^A�Q�A�V�A��A�E9A�zxA�kA��A�bA��XA��aA��PA���A���A��tA�NpA�g8A��A���A�3hA���A���A���A���A��"A���A��'A��aA�kA�t�A�	�A�� A�~�A�s�A��A�X�A�1�A��FA�}"A���A�aA���A��-A�%FA�HA�1�A�,=A�2�A��sA�� A�%A���A�k�A�A��A��A��~A�XEA�
�A�K�A?�A|ѷAu�<As�HAq|�Ao��An��An1'An*�Am�.Am6Ai�+Ag�"AgS�Ag�Af��Af2aAd�oAdP�Ad/�AdSAb��Aa7�A`4A_��A^�A]�vAX��ATیASo AQ�SAP=qAOe�AN �AL�AL�AJ�*AH�]AG��AG2�AF�hAF/�AE��AD�pAC�gAB��AA�uA@��A?�A=�A;��A;�wA;�LA;CA9�$A8��A7ffA6�A4��A2�A1�
A1��A1]�A1%A0a|A/��A.��A,�;A+0�A*A*��A)��A(�A(_pA'�MA&͟A%�XA$=qA#�{A#1�A"��A"�VA"�A� AZA�AK^A(�AL�A0�AQ�A/�A�BA \A�A�XA!�AE�A�A�sA��A��A�A
��A
��A
�A	zA	YKA	A�A�KA�dATaA9�A��A|�AoAC�A��A�	A�oA ��A 	@��@�xl@��^@�j�@�e@��@��+@���@��@�e@��t@�n/@�7L@��z@蠐@�zx@柾@��,@�{@�M�@߮�@ߚk@߀4@��P@��@�_p@ܽ<@�bN@�/�@��>@�c�@ڹ$@�!-@ע�@ֹ�@�1@ՍP@��@�7@��@�<�@�x�@�+@�z@���@�Vm@��@˅@��@ʕ@ɪ�@�;d@Ȋr@Ǡ'@�E�@�'�@Ę_@�k�@�1�@�A @�-w@�w�@�w2@���@�l�@�{J@���@���@�\�@���@��Z@���@�<�@���@��9@�@���@�C�@�qv@��
@��Z@��@���@�9X@���@�@�7@��@��
@��K@���@��@��~@��{@�A�@��@�6�@��@���@�a|@�>B@�/�@�C�@�
�@�|@�"h@�@��@�O�@�+@���@�s@��,@��b@��@��'@�l"@�_@��r@��@�خ@�H�@��F@��@�� @��0@�x@���@��O@�L0@���@�^�@�/�@��@���@���@���@�~(@���@���@�J#@��+@���@���@��a@��@��*@���@�}�@��@���@��x@�[�@���@�dZ@�@O@�J@�bN@�%@���@���@��+@�@�@��@��H@�C�@��U@��@�`�@�I�@�~(@��p@��@�X@�(�@���@�]d@��@��)@��@���@�l�@�4@��@��?@��b@���@�w�@�r�@�l�@�PH@��Z@��h@�F@��@���@��L@�p;@�h�@�J�@�%�@��@��
@��d@���@�f�@��@��@���@��s@��@�s�@�1@���@�F@���@��v@���@�ȴ@��9@�y>@�a|@�-�@��g@���@���@�=@���@��@�oi@��@���@�e�@�+@��I@�7@�@�]@��@~Q@|�p@|l"@|��@|��@|?�@|  @{/�@z^5@zR�@y��@xQ�@vl�@v6�@v@u��@tbN@s��@s�@q�~@p:�@p�@o�+@o��@n��@n$�@m�o@m�N@m��@mDg@l��@l'R@k�@k��@k�[@k˒@k�q@k��@l�@l$@lN�@lN�@l[�@k��@kdZ@j�@j�L@jR�@i�Z@i��@i�T@i��@i�~@h�@h��@h�@h�O@h��@h$@g��@g@O@f�h@fh
@f�@e�@e�@e��@eu�@d�)@dG@c��@cH�@b��@b��@b��@b��@bh
@bu@a�S@a(�@a�@`�_@`u�@`Q�@`$@_�V@_Mj@_]�@_b�@^�X@^
�@]��@\�p@\V�@\@[�6@[�K@[�$@Z��@Z��@Z�@Y�=@Y(�@X�v@X�@Wb�@W!-@W$t@W�@V҉@V�x@V��@V�x@V��@V��@VM�@V8�@V.�@V�@V�@V
�@U�@T��@TS�@S��@Sy�@S i@R�B@R��@R��@R	@R �@Q��@Q�S@P�j@P2�@Pb@O�@O��@O��@OA�@O1�@O&@N��@Nc @N$�@MG�@L��@L�@K��@K��@K��@K�@Kn/@K)_@J�@J�8@J��@J��@J)�@I8�@HQ�@G��@G�@@GF�@G�@F�"@F�,@F�<@F��@F�F@F~�@Fi�@F^5@FM�@F0U@F�@Fe@Fe@F@E�D@E�n@E��@E��@EIR@E�@D�@D�j@D�@Dq@D9X@C�V@C@B҉@B��@BJ�@B0U@B�@B_@A�@A��@Ak�@ADg@@�@@q@@�@?Z�@>�@>��@>i�@>:*@>.�@>�@>O@>4@>�@=�@=��@=�@=��@<��@;��@;J#@:��@9��@9�H@9x�@9A @9q@8�@8��@7�&@7�P@7�@6�@6��@6�x@6�A@6a|@6+k@6�@5��@5�@5�@5@5��@5IR@4�4@4�@4N�@3�w@3�	@3y�@3iD@3Z�@3S�@3/�@2Q@2{@2�@1�@1��@1(�@0�f@0�u@0 �@/�a@/�k@/�@.��@.�@.��@.s�@.d�@.&�@-�D@-�@-ϫ@-�'@-��@-��@-��@-��@-��@-�"@-hs@-%@,V�@,  @+ݘ@+ݘ@+ݘ@+��@+��@+H�@*�R@*��@*~�@*z@*{�@*p;@*p;@*i�@*V@*:*@*#:@)�.@)@)��@)�h@)w2@)O�@([�@'��@'g�@'�@&��@&�R@&��@&��@&��@&��@&}V@&a|@&;�@&�@%�>@%�@%c@%k�@%L�@$V�@#�@#,�@"��@"ȴ@"��@"c @!�.@!�z@!V@ ��@ �U@ N�@�*@�2@@@�@]d@��@� @dZ@�@�@�6@�F@�r@s�@p;@Z�@B[@0U@_@�N@��@��@p�@f�@`B@Vm@=�@ \@�I@�@@9�@/�@
=@{@�9@��@p�@B�@�[@	�@�@��@�6@�0@�P@s@X�@6z@
=@��@�@�@�M@�@�c@�c@��@��@�]@�]@�@�s@��@��@�+@V@$�@
�@�3@�?@N�@I�@?�@-�@%�@"h@�r@�&@�@�
@��@�@��@�$@��@b�@P�@6z@&@.I@1�@;d@C�@@O@,�@��@u%@i�@Ov@GE@;�@($@&�@�C@�@��@��@j@@q@�@�|@�$@~(@_@9X@7@g�@�@
��@
ȴ@
�@
�R@
��@
��@
��@
��@
�@
��@
}V@
v�@
Z�@
R�@
;�@
-@
#:@
@	��@	�@	��@	�N@	�@	�@	ϫ@	�@	��@	�t@	�@	��@	p�@	A @	�@ѷ@S�@(�@!@	�@G@��@dZ@��@�@^5@3�@!�@#:@��@�@�^@��@k�@[W@^�@N<@<6@(�@#�@#�@�@�K@�)@��@Q�@�g@]�@�@�@��@�]@ߤ@ں@�H@�@��@#:@m]@ ��@ bN?���?�H�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   BRBU2BT,BQ�BQ�BW?BiBq�BwBz�B|�B��B��B��B�SB��B��B��B��B�CB��B�%BB��B�Bw�B[�B0!B5B�B�B��B�B�7B֡BՁB҉BѷB�YB�B�kB׍B�B��B�B�MButBLdBB�B:B-B#�BxB�B��B�lB��B�]B�	B�zB��B�cB��B�B�bB��B��B��B�qB��B��BzxBh$B\CBT�BP�BJ�B,BVB�B�B
=B�B�oBݘB�B��B��B��B�B��Bz�Br�BmwBf�BP�B2�B�B\BB
��B
ðB
��B
�B
��B
�aB
�2B
��B
�VB
�;B
��B
��B
|�B
yXB
wB
t�B
j�B
b�B
a�B
b�B
fLB
UMB
PHB
KDB
A�B
?�B
&�B
�B
B	��B	�hB	�B	�_B	��B	ޞB	��B	��B	�:B	�4B	ЗB	��B	̘B	�B	żB	� B	��B	�%B	�'B	��B	�B	�_B	��B	�B	��B	�B	��B	��B	�B	~�B	{�B	{JB	{B	xB	t9B	m)B	i�B	fLB	`�B	^�B	\�B	]~B	X�B	W�B	T�B	S�B	R�B	K)B	F%B	C�B	BAB	@ B	?}B	:B	6zB	5%B	5�B	1�B	,�B	($B	%B	"�B	 \B	/B	�B	�B	B	eB	aB	�B	vB	
=B	EB	�B	{B	-B	�B	 B	 �B	oB	 B��B�B��B�6B�B��B��B��B��B�B�B�B�B�B��B�B��B�"B��B�WB�B��B�>B�mB�>B�RB�LB�B�B�B�fB��B�B��B�B�8B�B��B�LB�zB�B�B�B�B��B��B�'B�pB��B��B�B�BߊB�pBߤBߤB�!B��BߊBޞB޸BބB�B�OBݘB�B�B��B�;B��B�\B��B�NB�LB�_B�B��B	B�^B�B�6B��B�B��B�B	 �B	 iB	?B	B	B	�B	%,B	(>B	.B	-B	,�B	/ B	0�B	1�B	2|B	3hB	4nB	4�B	5�B	6B	7�B	8RB	9�B	9XB	:�B	<�B	>�B	?cB	C�B	ESB	F�B	KB	NpB	P�B	S&B	S[B	U�B	ZQB	Z7B	Z�B	^�B	_VB	e�B	f�B	gB	gB	gRB	i*B	i�B	kB	k�B	k�B	m]B	oiB	t�B	u�B	xB	z�B	{dB	{�B	|�B	|�B	|PB	}B	~�B	�B	��B	��B	�+B	�B	��B	��B	��B	�VB	��B	�TB	�&B	��B	�`B	�_B	�CB	�/B	�UB	�!B	��B	��B	�[B	��B	��B	��B	�5B	�CB	�]B	��B	�OB	�iB	�B	�B	�zB	�RB	�jB	�}B	��B	��B	ԕB	յB	֡B	��B	ۦB	��B	�bB	�4B	�:B	�B	��B	�ZB	�B	�2B	�
B	�DB	�eB	��B	�OB	��B	�'B	�aB	�B	�XB	��B	�^B	��B	��B
�B
�B
�B
�B
�B
B

rB
JB
"B
�B
4B
�B
�B
{B
B
�B
�B
B
7B
�B
xB
IB
dB
jB
pB
 BB
!bB
$�B
(�B
1B
2�B
3�B
5�B
8B
8RB
8�B
;�B
?�B
C-B
EB
F�B
IB
I�B
MB
O�B
S�B
TB
T�B
UgB
Y�B
[=B
\�B
]/B
\�B
^B
_!B
`�B
cB
f�B
f�B
gB
g�B
hsB
iyB
j�B
lB
o B
q�B
u�B
w�B
y	B
y�B
z�B
{�B
}VB
}B
~BB
�B
��B
�aB
�B
��B
�?B
�tB
��B
��B
�lB
��B
��B
��B
��B
��B
��B
�"B
�vB
��B
��B
��B
�B
�,B
��B
��B
��B
�B
��B
�]B
�xB
�]B
��B
�IB
��B
�B
�bB
��B
��B
��B
�@B
��B
��B
�8B
��B
�XB
�B
��B
��B
�;B
��B
�vB
�B
��B
��B
��B
��B
�>B
�^B
�JB
��B
�(B
�cB
��B
��B
�B
��B
� B
�;B
�B
��B
�;B
��B
��B
��B
�'B
��B
��B
�MB
�B
��B
ɆB
��B
��B
�~B
̳B
�B
͟B
��B
�<B
�B
οB
οB
�B
�(B
�BB
��B
�.B
�B
�B
��B
бB
� B
�&B
�B
��B
յB
�mB
�YB
خB
�KB
��B
�QB
�kB
چB
ڠB
چB
یB
��B
�B
޸B
�!B
�!B
�VB
ߤB
��B
�B
�BB
�vB
�B
��B
��B
�bB
�B
�4B
�B
�B
�B
�B
�B
�tB
�@B
�@B
�ZB
�B
�zB
��B
�fB
�
B
�yB
�0B
�B
��B
�B
�/B
�B
�B
�B
�B
��B
��B
�B
�MB
�B
�%B
��B
��B
��B
�B
�FB
�+B
�`B
�`B
�zB
�zB
��B
��B
�RB
�rB
��B
�B
��B
�"B
��B
�B
�BB
��B
�.B �B B�BAB�B�B�BB{B�B3BBMB�B�B9B�B�B�B�B�B	7B	RB	lB	RB	�B^BxBxB�B�BBB�BpB�B�BHB�B�B�B BB�B�BBBoBoBoBoBoB�BoB�B@BaB�B2B�B�B�BBB�B$B$B$B$BYB?B?BYBsB�B�BEB_ByB�B�B�BWB�BBxB�B�B�B�BB�B/B~B�BB5B�BjBjB BB!B!�B"B"NB"NB"�B#:B#nB$�B$�B$�B%`B&LB'RB)*B)�B*B*�B+QB+kB,=B,qB,�B-CB-CB-]B-wB-wB-�B-�B-�B.B.IB.�B.�B.�B/ B/ B/B/B/B/�B1vB1�B1�B1�B3B3�B4B4B4B5%B6+B5�B6B6FB6`B6�B6�B6�B7B7fB7�B7�B7�B7�B7�B7�B7�B7�B7�B7�B7�B7�B7�B7�B7�B8RB8�B8�B8�B9	B:�B;JB;JB;dB;B;dB;B;�B;�B;�B<B<B<B<B<PB<jB<�B<�B<�B=B<�B<�B<�B<�B<�B<�B=�B=�B>B>(B>BB>BB>]B>BB?HB?.B?HB?HB?cB@ B?�B?�B@B@iB@�B@�B@�BABBABB�BB�BB�BCBCBC-BC-BC-BCGBCaBC{BC{BC�BC�BC�BC�BC�BC�BDMBDMBDMBDgBD�BD�BD�BD�BD�BD�BD�BD�BD�BEBESBE�BE�BF�BF�BF�BF�BF�BGBG�BH�BIBI7BIlBI�BI�BI�BI�BJ#BJrBJ�BJ�BJ�BJ�BJ�BJ�BJ�BJ�BKBKDBK^BK�BLBL�BM�BM�BM�BM�BN"BM�BNBM�BNBM�BN�BO�BP�BQ4BRBQ�444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444   BRBU2BT,BQ�BQ�BW?BiBq�BwBz�B|�B��B��B��B�SB��B��B��B��B�CB��B�%BB��B�Bw�B[�B0!B5B�B�B��B�B�7B֡BՁB҉BѷB�YB�B�kB׍B�B��B�B�MButBLdBB�B:B-B#�BxB�B��B�lB��B�]B�	B�zB��B�cB��B�B�bB��B��B��B�qB��B��BzxBh$B\CBT�BP�BJ�B,BVB�B�B
=B�B�oBݘB�B��B��B��B�B��Bz�Br�BmwBf�BP�B2�B�B\BB
��B
ðB
��B
�B
��B
�aB
�2B
��B
�VB
�;B
��B
��B
|�B
yXB
wB
t�B
j�B
b�B
a�B
b�B
fLB
UMB
PHB
KDB
A�B
?�B
&�B
�B
B	��B	�hB	�B	�_B	��B	ޞB	��B	��B	�:B	�4B	ЗB	��B	̘B	�B	żB	� B	��B	�%B	�'B	��B	�B	�_B	��B	�B	��B	�B	��B	��B	�B	~�B	{�B	{JB	{B	xB	t9B	m)B	i�B	fLB	`�B	^�B	\�B	]~B	X�B	W�B	T�B	S�B	R�B	K)B	F%B	C�B	BAB	@ B	?}B	:B	6zB	5%B	5�B	1�B	,�B	($B	%B	"�B	 \B	/B	�B	�B	B	eB	aB	�B	vB	
=B	EB	�B	{B	-B	�B	 B	 �B	oB	 B��B�B��B�6B�B��B��B��B��B�B�B�B�B�B��B�B��B�"B��B�WB�B��B�>B�mB�>B�RB�LB�B�B�B�fB��B�B��B�B�8B�B��B�LB�zB�B�B�B�B��B��B�'B�pB��B��B�B�BߊB�pBߤBߤB�!B��BߊBޞB޸BބB�B�OBݘB�B�B��B�;B��B�\B��B�NB�LB�_B�B��B	B�^B�B�6B��B�B��B�B	 �B	 iB	?B	B	B	�B	%,B	(>B	.B	-B	,�B	/ B	0�B	1�B	2|B	3hB	4nB	4�B	5�B	6B	7�B	8RB	9�B	9XB	:�B	<�B	>�B	?cB	C�B	ESB	F�B	KB	NpB	P�B	S&B	S[B	U�B	ZQB	Z7B	Z�B	^�B	_VB	e�B	f�B	gB	gB	gRB	i*B	i�B	kB	k�B	k�B	m]B	oiB	t�B	u�B	xB	z�B	{dB	{�B	|�B	|�B	|PB	}B	~�B	�B	��B	��B	�+B	�B	��B	��B	��B	�VB	��B	�TB	�&B	��B	�`B	�_B	�CB	�/B	�UB	�!B	��B	��B	�[B	��B	��B	��B	�5B	�CB	�]B	��B	�OB	�iB	�B	�B	�zB	�RB	�jB	�}B	��B	��B	ԕB	յB	֡B	��B	ۦB	��B	�bB	�4B	�:B	�B	��B	�ZB	�B	�2B	�
B	�DB	�eB	��B	�OB	��B	�'B	�aB	�B	�XB	��B	�^B	��B	��B
�B
�B
�B
�B
�B
B

rB
JB
"B
�B
4B
�B
�B
{B
B
�B
�B
B
7B
�B
xB
IB
dB
jB
pB
 BB
!bB
$�B
(�B
1B
2�B
3�B
5�B
8B
8RB
8�B
;�B
?�B
C-B
EB
F�B
IB
I�B
MB
O�B
S�B
TB
T�B
UgB
Y�B
[=B
\�B
]/B
\�B
^B
_!B
`�B
cB
f�B
f�B
gB
g�B
hsB
iyB
j�B
lB
o B
q�B
u�B
w�B
y	B
y�B
z�B
{�B
}VB
}B
~BB
�B
��B
�aB
�B
��B
�?B
�tB
��B
��B
�lB
��B
��B
��B
��B
��B
��B
�"B
�vB
��B
��B
��B
�B
�,B
��B
��B
��B
�B
��B
�]B
�xB
�]B
��B
�IB
��B
�B
�bB
��B
��B
��B
�@B
��B
��B
�8B
��B
�XB
�B
��B
��B
�;B
��B
�vB
�B
��B
��B
��B
��B
�>B
�^B
�JB
��B
�(B
�cB
��B
��B
�B
��B
� B
�;B
�B
��B
�;B
��B
��B
��B
�'B
��B
��B
�MB
�B
��B
ɆB
��B
��B
�~B
̳B
�B
͟B
��B
�<B
�B
οB
οB
�B
�(B
�BB
��B
�.B
�B
�B
��B
бB
� B
�&B
�B
��B
յB
�mB
�YB
خB
�KB
��B
�QB
�kB
چB
ڠB
چB
یB
��B
�B
޸B
�!B
�!B
�VB
ߤB
��B
�B
�BB
�vB
�B
��B
��B
�bB
�B
�4B
�B
�B
�B
�B
�B
�tB
�@B
�@B
�ZB
�B
�zB
��B
�fB
�
B
�yB
�0B
�B
��B
�B
�/B
�B
�B
�B
�B
��B
��B
�B
�MB
�B
�%B
��B
��B
��B
�B
�FB
�+B
�`B
�`B
�zB
�zB
��B
��B
�RB
�rB
��B
�B
��B
�"B
��B
�B
�BB
��B
�.B �B B�BAB�B�B�BB{B�B3BBMB�B�B9B�B�B�B�B�B	7B	RB	lB	RB	�B^BxBxB�B�BBB�BpB�B�BHB�B�B�B BB�B�BBBoBoBoBoBoB�BoB�B@BaB�B2B�B�B�BBB�B$B$B$B$BYB?B?BYBsB�B�BEB_ByB�B�B�BWB�BBxB�B�B�B�BB�B/B~B�BB5B�BjBjB BB!B!�B"B"NB"NB"�B#:B#nB$�B$�B$�B%`B&LB'RB)*B)�B*B*�B+QB+kB,=B,qB,�B-CB-CB-]B-wB-wB-�B-�B-�B.B.IB.�B.�B.�B/ B/ B/B/B/B/�B1vB1�B1�B1�B3B3�B4B4B4B5%B6+B5�B6B6FB6`B6�B6�B6�B7B7fB7�B7�B7�B7�B7�B7�B7�B7�B7�B7�B7�B7�B7�B7�B7�B8RB8�B8�B8�B9	B:�B;JB;JB;dB;B;dB;B;�B;�B;�B<B<B<B<B<PB<jB<�B<�B<�B=B<�B<�B<�B<�B<�B<�B=�B=�B>B>(B>BB>BB>]B>BB?HB?.B?HB?HB?cB@ B?�B?�B@B@iB@�B@�B@�BABBABB�BB�BB�BCBCBC-BC-BC-BCGBCaBC{BC{BC�BC�BC�BC�BC�BC�BDMBDMBDMBDgBD�BD�BD�BD�BD�BD�BD�BD�BD�BEBESBE�BE�BF�BF�BF�BF�BF�BGBG�BH�BIBI7BIlBI�BI�BI�BI�BJ#BJrBJ�BJ�BJ�BJ�BJ�BJ�BJ�BJ�BKBKDBK^BK�BLBL�BM�BM�BM�BM�BN"BM�BNBM�BNBM�BN�BO�BP�BQ4BRBQ�444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220816094904  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220816094906  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220816094907  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220816094907                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220816184912  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220816184912  QCF$                G�O�G�O�G�O�            8000JA  ARUP                                                                        20220816095931                      G�O�G�O�G�O�                