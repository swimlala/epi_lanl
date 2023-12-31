CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-09T12:49:45Z creation;2022-06-09T12:49:47Z conversion to V3.1      
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
_FillValue                 �  I0   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ph   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tT   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �<   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �(   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  Έ   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �8   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �h   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �h   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �h   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �h   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20220609124945  20220609130031  4902988                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               aA   JA                                  2B  A   APEX                            8600                            2.11.3                          846 @���E�=1   @���w`@<�7KƧ��c�;dZ�1   GPS     A   A   F   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @�ff@�33A��A   A@  Aa��A�  A�33A�33A�33A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���C  C  C�fC  C
�C�C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx�Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C��3C��3C�  C�  C��C�  C��3C��C��C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C��C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C��3C��3C�  C�  C�  C�  C�  C��C�  C�  C��C�  C��3C�  C�  C�  C��C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C��3C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C��C��D   D y�D ��Dy�D  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D   D � D!  D!y�D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(y�D)  D)� D*  D*� D+  D+� D,  D,�fD-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8fD8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� D@��DA� DB  DB� DC  DC� DD  DD� DEfDE�fDF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DXy�DX��DY� DZ  DZ� D[  D[� D\  D\� D]  D]y�D]��D^� D_  D_� D`fD`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj�fDk  Dk� Dl  Dl� Dm  Dm� Dn  Dny�Do  Do�fDp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt��Du� DvfDv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� Dz��D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�<�D�|�D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�<�D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�3D�@ D�|�D�� D�  D�@ D�� D�� D�  D�C3D��3D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D��3D�� D���D�@ D��3D��3D�  D�@ D�|�D���D�  D�@ D�� D�� D���D�@ D��3D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�<�DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D���D�<�DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D���D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�3D�@ Dڀ Dڼ�D�  D�@ Dۀ D��3D�3D�@ D܀ D��3D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D��3D�  D�@ D� D��D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�C3D�3D�� D�  D�C3D�3D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�C3D� D�� D�  D�C3D�3D�� D�  D�<�D� D�� D�3D�@ D� D�� D�  D�@ D� D�� D���D�@ D�� D�� D�  D�@ D� D�� D���D�@ D� D�� D�  D�@ D� D��3D�  D�<�D� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�6f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��R@ÅAA (�A@(�AaA�{A�G�A�G�A�G�A�{A�{A�{A�{B 
=B
=B
=B
=B 
=B(
=B0
=B8
=B@
=BH
=BP
=BX
=B`
=Bh
=Bp
=Bx
=B�B�B�B�B�B�B�B�B�B�B�8RB�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B���C�C�C��C�C
)C)C�C�C�C�C�C�C�C�C�C �C"�C$�C&�C(�C*�C,�C.�C0�C2�C4�C6�C8�C:�C<�C>�C@�CB�CD�CF�CH�CJ�CL�CN�CP�CR�CT�CV�CX�CZ�C\�C^�C`�Cb�Cd�Cf�Ch�Cj�Cl�Cn�Cp�Cr�Ct�Cv�Cx)Cz�C|�C~�C�HC�HC�HC�HC�HC�HC�HC�HC�HC��{C�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�C�HC��{C��{C�HC�HC�C�HC��{C�C�C�HC��{C��{C�HC�HC�HC�HC�HC�HC�HC��{C��{C�HC�HC�C�HC�HC�HC�HC�HC��{C�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�C�HC�HC��{C��{C�HC�HC�HC�HC�HC�C�HC�HC�C�HC��{C�HC�HC�HC�C�HC�HC��{C�HC�HC�HC�HC�HC�HC�HC��{C�HC�HC�HC�HC�HC��{C�HC�C�HC�HC�HC�HC�HC�HC�HC�HC�C�D  �D z>D �>Dz>D �D��D �D��D �D��D �D��D �D��D �Dz>D �D��D	 �D	��D
 �D
��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D�>D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �Dz>D �D��D  �D ��D! �D!z>D" �D"��D# �D#��D$ �D$��D% �D%��D& �D&��D' �D'��D( �D(z>D) �D)��D* �D*��D+ �D+��D, �D,�
D- �D-��D. �D.��D/ �D/��D0 �D0��D1 �D1��D2 �D2��D3 �D3��D4 �D4��D5 �D5��D6 �D6��D7 �D7��D8
D8��D9 �D9��D: �D:��D; �D;��D< �D<��D= �D=��D> �D>��D? �D?��D@ �D@��D@�>DA��DB �DB��DC �DC��DD �DD��DE
DE�
DF �DF��DG �DG��DH �DH��DI �DI��DJ �DJ��DK �DK��DL �DL��DM �DM��DN �DN��DO �DO��DP �DP��DQ �DQ��DR �DR��DS �DS��DT �DT��DU �DU��DV �DV��DW �DW��DX �DXz>DX�>DY��DZ �DZ��D[ �D[��D\ �D\��D] �D]z>D]�>D^��D_ �D_��D`
D`��Da �Da��Db �Db��Dc �Dc��Dd �Dd��De �De��Df �Df��Dg �Dg��Dh �Dh��Di �Di��Dj �Dj�
Dk �Dk��Dl �Dl��Dm �Dm��Dn �Dnz>Do �Do�
Dp �Dp��Dq �Dq��Dr �Dr��Ds �Ds��Dt �Dt��Dt�>Du��Dv
Dv��Dw �Dw��Dx �Dx��Dy �Dy��Dz �Dz��Dz�>D{��D| �D|��D} �D}��D~ �D~��D �D��D� RD�C�D��RD��RD� RD�@RD��RD��RD� RD�@RD�}D��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD���D��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD�ÅD� RD�=D�}D��D� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD��D�=D�}D��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD�ÅD� RD�@RD��RD��RD� RD�C�D��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��D� RD�@RD��RD��RD��D�@RD�}D��RD� RD�@RD��RD��RD� RD�C�D���D��RD� RD�@RD��RD��RD��D�@RD��RD��RD� RD�C�D��RD��RD� RD�@RD���D��RD��D�@RD���D�ÅD� RD�@RD�}D��D� RD�@RD��RD��RD��D�@RD���D�ÅD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�C�D��RD��RD��D�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD��D�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�=D�}D��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�C�D��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�=D��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RDRD��RD� RD�=DÀRD��RD� RD�@RDĀRD��RD� RD�@RDŀRD��RD� RD�@RDƀRD��RD� RD�@RDǀRD��RD� RD�@RDȀRD��RD� RD�@RDɀRD��RD� RD�@RDʀRD��RD� RD�@RDˀRD��RD� RD�@RD̀RD��RD� RD�@RD̀RD��RD� RD�@RD΀RD��RD� RD�@RDπRD��RD� RD�@RDЀRD��RD� RD�@RDрRD��RD� RD�@RDҀRD��RD��D�=DӀRD��RD� RD�@RDԀRD��RD� RD�@RDՀRD��RD� RD�@RDրRD��RD��D�@RD׀RD��RD� RD�@RD؀RD��RD� RD�@RDـRD��RD��D�@RDڀRDڽD� RD�@RDۀRD�ÅD��D�@RD܀RD�ÅD� RD�@RD݀RD��RD� RD�@RDހRD��RD� RD�@RD߀RD��RD� RD�@RD��RD��RD� RD�@RD�RD�ÅD� RD�@RD�RD�D� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�C�D烅D��RD� RD�C�D胅D��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�C�D�RD��RD� RD�C�D샅D��RD� RD�=D�RD��RD��D�@RD�RD��RD� RD�@RD�RD��RD��D�@RD��RD��RD� RD�@RD�RD��RD��D�@RD�RD��RD� RD�@RD�RD�ÅD� RD�=D�RD�ÅD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�6�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�ZA�z�A���AϼAω�A�SA͜A�o�A���A�'�A���A���A�9�AÓA�רA�xlA��)A��A�A��A��xA�� A���A�5�A�+A�F?A��A�)�A���A���A���A�%FA�ٴA�ffA��$A�A�+A���A�!A�PHA��A�r�A��A�W
A���A�'�A�v+A��kA�x8A���A�cTA���A��ZA�8�A�c�A��A�>�A���A�:*A�pA��A��A�8�A��A���A���A�v+A���A�GA�gmA�ffA��iA��?A��A���A�R A�qA��A�f�A��#A�b�A�!bA��wA�{�A��A�YA��A���A�OA��_A�'�A�IRA�RTA�� A�E�A���A���A�@OA���A�uA~�MA}v`Ay��AwTaAu��At��AtoiAr��Ap˒Ao;�An��An	Am8�Aj�uAi�Ah�hAh<6Ag�)Agu%Af�rAd��Ac`BAb��Aa�EA`��A^خA]2�A\.�A[��AZ�xAY_�AX�;AWl�AVZATS�ASp�AR0UAQ��AQE9AP�~AP=�AP�AN��AMH�AK��AK�	AJ�RAH�NAG��AG��AGW?AGK�AG1�AF�AFtTAE��AE��AE�AC?}AB�HAA$A@�HA?:�A>oiA=��A=J�A;��A:��A9�A85?A7S&A5��A4bNA3�A3A�A3�A2�tA2$tA2	lA1��A1l�A0��A/�A.VA,�6A,r�A,~A+xA*��A*��A)�A'�A'�*A'sA'{A&��A%l�A#�IA"��A!�)A!qvA!�A ��A ��A ZA4�A%�A�'Af�A�5A\�A�8A�^AF�A1Ag�A�A��A�]AL�A�A,�A�}AS�AA��A�WA�A��Ah�A,�A�&Am�A�aA+AA�DAK�A֡A�Ac A��AH�A
� A
�A	!�A��A/A��A�rA�PA�UAU�A�	A��A�5@���@�9X@��@��@��@�s@���@�Z@��F@�y�@���@��K@���@�@��?@�Mj@�خ@�$@�  @��m@�4@��@�L0@��|@�dZ@�GE@�  @���@�9X@�F�@֥z@�=q@��>@�e�@Ԡ�@���@��@�[�@͋�@�z�@���@˂�@�]�@��2@ɷ@Ȳ�@Ǘ$@Ŕ�@Ĝx@�Ov@���@ÍP@°!@��k@��@��@��}@�a@��@��@�L0@��@��@��@@���@���@�C-@��a@�Vm@��@�@�g�@��@�]d@�]�@��9@�0U@��4@�%F@��,@�C�@��|@�_@�e,@��@�"h@�˒@���@�iD@�J#@�@�?�@�^�@��D@��@�x@�zx@���@�Ta@���@�9X@��@��b@��m@�G�@��}@�.�@���@���@�ں@��m@�h�@�#�@��$@���@�+@�n�@�V�@�<�@�($@��W@���@�@��M@��@��1@�7L@���@�m�@�i�@�i�@��U@�a�@��'@�t�@���@�h�@�{@���@���@�_@��K@�\)@���@��m@�E�@��w@��@���@�Mj@��y@�@�rG@��a@��C@�o @���@�<�@�n/@��|@���@�`�@�W�@�K^@�-�@��@��@��$@���@���@���@�R�@���@�"�@���@���@��}@���@�-@��)@��d@��[@��M@�p�@�O�@�J�@�H�@�6z@��@���@���@��@��$@��D@�l�@�3�@��@��^@��*@�x�@��@���@�u%@�=q@�2�@��@J#@9�@�@~�A@~Q@~6�@}��@|[�@{�w@z~�@y��@y@@x�_@xN�@x<�@x �@x�@x!@w��@w\)@v�@v	@u�d@u*0@t9X@s�@s��@sU�@r�<@r�x@r�\@r��@r�b@r�1@r��@q��@qc�@q�@q�@p֡@o��@n@m&�@mO�@mԕ@n!�@m��@mm]@m�@l<�@l�@k�@k��@j��@jz@j�@i��@i%@h�@g��@g
=@f��@f͟@fu@eq@d�@d��@d��@dV�@c��@cqv@cE9@c9�@bR�@a��@am]@aS&@a%F@`�K@`�@`h�@`@_��@_j�@_O@_.I@^� @^p;@^ff@^c @^_�@]�@]rG@]IR@]&�@]	l@\�@\�`@\��@\��@\��@\y>@\7�@[�:@[A�@Z�}@ZZ�@Z($@Z�@Y��@Y�@Y�H@Y��@Yj@Y+�@X�.@W�m@W�:@W6z@V�y@V�!@V�]@Vں@Vz@U��@Up�@T�@Tu�@T9X@S�
@S��@S��@Sg�@S.I@SY@S(@R��@R҉@R��@Rc @Q�@Q�z@Q��@P�@P$@O��@O�
@O�6@O�[@O�@O{J@O1�@O�@N��@N�H@N�@N��@N��@N1�@Mϫ@M��@MrG@M[W@Mq@L��@L�@L�j@L�e@Le�@L~@K��@K��@K$t@K@K�@J�M@J�H@J�s@J��@JW�@I�)@I��@Ia�@IA @I�@H�@HZ@G��@G�F@GdZ@F�@F�'@F�@FkQ@F5?@F�@E��@D�@D�@Cƨ@C��@C�0@C�0@C��@C�$@Cn/@CX�@C>�@B��@Bȴ@B��@B��@B��@B��@B��@B�R@B��@Bff@A�@A�~@ArG@A^�@AA @A�@A�@@Ɇ@@Xy@@2�@@/�@@(�@@2�@@9X@?�@?�4@?;d@>��@>��@>\�@>�@=��@=zx@=S&@=?}@=@=�@=	l@<Ɇ@< �@;�*@;�@:�m@:�R@:�}@:� @:xl@:Ta@:?@:+k@:O@9��@9��@9c�@9+@8�@8�u@8(�@7��@7e�@7�@6��@6i�@6YK@6;�@64@5�@5m]@5Dg@5+@5�@4��@4�@4�E@4�[@4��@4H@4>B@4M@4M@4-�@4G@3�@3��@3��@3_p@31�@3�@2�B@2u%@2h
@2^5@2$�@1Vm@0֡@0�)@0��@0��@0��@0:�@/��@/e�@/=@.�M@.�@.z@.0U@-�@-��@-�@-c�@-�@,Ɇ@,�@,h�@,*�@+�;@+��@+W?@+�@*��@*��@*H�@)�Z@)��@)��@)�~@)m]@)5�@(�/@(��@(:�@(�@'�r@'�@&�"@&h
@&Ov@&�@%�H@%�@$��@$g8@$%�@#��@#�$@#��@#��@#;d@"ȴ@"�x@"�A@!��@!�M@!O�@!%@ ��@ c�@ G@�A@�@��@�K@�f@�@@�@�o@�9@�z@�@rG@�@�5@�@��@�z@M@M@�@�V@�P@�	@RT@$t@�@��@z@H�@3�@0U@&�@��@��@�t@m]@�@��@�/@Ĝ@�j@��@9X@M@G@  @�@��@�
@�K@�@��@�q@�	@b�@U�@>�@4�@�@�@�}@��@�A@s�@d�@W�@8�@�)@�H@4@��@�@��@�@~(@u�@g8@_@Ft@7�@*�@7@b@	�@�@�+@�@� @l�@(@ߤ@ȴ@��@��@��@��@�r@xl@8�@�.@�@ϫ@��@�'@�@\�@%F@��@ی@֡@�p@�9@�_@w�@?�@7@�@��@��@@O@��@�m@�A@n�@L0@B[@3�@�o@�N@�t@��@�@rG@\�@<6@�@�I@l"@Ft@7@��@�W@�w@�@l�@J#@,�@C@�@
��@
��@
�@
��@
ߤ@
�@
�@
��@
�x@
{�@
.�@
�@	�@	`B@	0�@	%F@	@@�@��@_@Q�@@�F@��@��@�f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�ZA�z�A���AϼAω�A�SA͜A�o�A���A�'�A���A���A�9�AÓA�רA�xlA��)A��A�A��A��xA�� A���A�5�A�+A�F?A��A�)�A���A���A���A�%FA�ٴA�ffA��$A�A�+A���A�!A�PHA��A�r�A��A�W
A���A�'�A�v+A��kA�x8A���A�cTA���A��ZA�8�A�c�A��A�>�A���A�:*A�pA��A��A�8�A��A���A���A�v+A���A�GA�gmA�ffA��iA��?A��A���A�R A�qA��A�f�A��#A�b�A�!bA��wA�{�A��A�YA��A���A�OA��_A�'�A�IRA�RTA�� A�E�A���A���A�@OA���A�uA~�MA}v`Ay��AwTaAu��At��AtoiAr��Ap˒Ao;�An��An	Am8�Aj�uAi�Ah�hAh<6Ag�)Agu%Af�rAd��Ac`BAb��Aa�EA`��A^خA]2�A\.�A[��AZ�xAY_�AX�;AWl�AVZATS�ASp�AR0UAQ��AQE9AP�~AP=�AP�AN��AMH�AK��AK�	AJ�RAH�NAG��AG��AGW?AGK�AG1�AF�AFtTAE��AE��AE�AC?}AB�HAA$A@�HA?:�A>oiA=��A=J�A;��A:��A9�A85?A7S&A5��A4bNA3�A3A�A3�A2�tA2$tA2	lA1��A1l�A0��A/�A.VA,�6A,r�A,~A+xA*��A*��A)�A'�A'�*A'sA'{A&��A%l�A#�IA"��A!�)A!qvA!�A ��A ��A ZA4�A%�A�'Af�A�5A\�A�8A�^AF�A1Ag�A�A��A�]AL�A�A,�A�}AS�AA��A�WA�A��Ah�A,�A�&Am�A�aA+AA�DAK�A֡A�Ac A��AH�A
� A
�A	!�A��A/A��A�rA�PA�UAU�A�	A��A�5@���@�9X@��@��@��@�s@���@�Z@��F@�y�@���@��K@���@�@��?@�Mj@�خ@�$@�  @��m@�4@��@�L0@��|@�dZ@�GE@�  @���@�9X@�F�@֥z@�=q@��>@�e�@Ԡ�@���@��@�[�@͋�@�z�@���@˂�@�]�@��2@ɷ@Ȳ�@Ǘ$@Ŕ�@Ĝx@�Ov@���@ÍP@°!@��k@��@��@��}@�a@��@��@�L0@��@��@��@@���@���@�C-@��a@�Vm@��@�@�g�@��@�]d@�]�@��9@�0U@��4@�%F@��,@�C�@��|@�_@�e,@��@�"h@�˒@���@�iD@�J#@�@�?�@�^�@��D@��@�x@�zx@���@�Ta@���@�9X@��@��b@��m@�G�@��}@�.�@���@���@�ں@��m@�h�@�#�@��$@���@�+@�n�@�V�@�<�@�($@��W@���@�@��M@��@��1@�7L@���@�m�@�i�@�i�@��U@�a�@��'@�t�@���@�h�@�{@���@���@�_@��K@�\)@���@��m@�E�@��w@��@���@�Mj@��y@�@�rG@��a@��C@�o @���@�<�@�n/@��|@���@�`�@�W�@�K^@�-�@��@��@��$@���@���@���@�R�@���@�"�@���@���@��}@���@�-@��)@��d@��[@��M@�p�@�O�@�J�@�H�@�6z@��@���@���@��@��$@��D@�l�@�3�@��@��^@��*@�x�@��@���@�u%@�=q@�2�@��@J#@9�@�@~�A@~Q@~6�@}��@|[�@{�w@z~�@y��@y@@x�_@xN�@x<�@x �@x�@x!@w��@w\)@v�@v	@u�d@u*0@t9X@s�@s��@sU�@r�<@r�x@r�\@r��@r�b@r�1@r��@q��@qc�@q�@q�@p֡@o��@n@m&�@mO�@mԕ@n!�@m��@mm]@m�@l<�@l�@k�@k��@j��@jz@j�@i��@i%@h�@g��@g
=@f��@f͟@fu@eq@d�@d��@d��@dV�@c��@cqv@cE9@c9�@bR�@a��@am]@aS&@a%F@`�K@`�@`h�@`@_��@_j�@_O@_.I@^� @^p;@^ff@^c @^_�@]�@]rG@]IR@]&�@]	l@\�@\�`@\��@\��@\��@\y>@\7�@[�:@[A�@Z�}@ZZ�@Z($@Z�@Y��@Y�@Y�H@Y��@Yj@Y+�@X�.@W�m@W�:@W6z@V�y@V�!@V�]@Vں@Vz@U��@Up�@T�@Tu�@T9X@S�
@S��@S��@Sg�@S.I@SY@S(@R��@R҉@R��@Rc @Q�@Q�z@Q��@P�@P$@O��@O�
@O�6@O�[@O�@O{J@O1�@O�@N��@N�H@N�@N��@N��@N1�@Mϫ@M��@MrG@M[W@Mq@L��@L�@L�j@L�e@Le�@L~@K��@K��@K$t@K@K�@J�M@J�H@J�s@J��@JW�@I�)@I��@Ia�@IA @I�@H�@HZ@G��@G�F@GdZ@F�@F�'@F�@FkQ@F5?@F�@E��@D�@D�@Cƨ@C��@C�0@C�0@C��@C�$@Cn/@CX�@C>�@B��@Bȴ@B��@B��@B��@B��@B��@B�R@B��@Bff@A�@A�~@ArG@A^�@AA @A�@A�@@Ɇ@@Xy@@2�@@/�@@(�@@2�@@9X@?�@?�4@?;d@>��@>��@>\�@>�@=��@=zx@=S&@=?}@=@=�@=	l@<Ɇ@< �@;�*@;�@:�m@:�R@:�}@:� @:xl@:Ta@:?@:+k@:O@9��@9��@9c�@9+@8�@8�u@8(�@7��@7e�@7�@6��@6i�@6YK@6;�@64@5�@5m]@5Dg@5+@5�@4��@4�@4�E@4�[@4��@4H@4>B@4M@4M@4-�@4G@3�@3��@3��@3_p@31�@3�@2�B@2u%@2h
@2^5@2$�@1Vm@0֡@0�)@0��@0��@0��@0:�@/��@/e�@/=@.�M@.�@.z@.0U@-�@-��@-�@-c�@-�@,Ɇ@,�@,h�@,*�@+�;@+��@+W?@+�@*��@*��@*H�@)�Z@)��@)��@)�~@)m]@)5�@(�/@(��@(:�@(�@'�r@'�@&�"@&h
@&Ov@&�@%�H@%�@$��@$g8@$%�@#��@#�$@#��@#��@#;d@"ȴ@"�x@"�A@!��@!�M@!O�@!%@ ��@ c�@ G@�A@�@��@�K@�f@�@@�@�o@�9@�z@�@rG@�@�5@�@��@�z@M@M@�@�V@�P@�	@RT@$t@�@��@z@H�@3�@0U@&�@��@��@�t@m]@�@��@�/@Ĝ@�j@��@9X@M@G@  @�@��@�
@�K@�@��@�q@�	@b�@U�@>�@4�@�@�@�}@��@�A@s�@d�@W�@8�@�)@�H@4@��@�@��@�@~(@u�@g8@_@Ft@7�@*�@7@b@	�@�@�+@�@� @l�@(@ߤ@ȴ@��@��@��@��@�r@xl@8�@�.@�@ϫ@��@�'@�@\�@%F@��@ی@֡@�p@�9@�_@w�@?�@7@�@��@��@@O@��@�m@�A@n�@L0@B[@3�@�o@�N@�t@��@�@rG@\�@<6@�@�I@l"@Ft@7@��@�W@�w@�@l�@J#@,�@C@�@
��@
��@
�@
��@
ߤ@
�@
�@
��@
�x@
{�@
.�@
�@	�@	`B@	0�@	%F@	@@�@��@_@Q�@@�F@��@��@�f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B�B�nB�B�B�AB� B�mB��B�CB�dB�hB�,B�B�)B��B�B��B�TBΊB�fB��BѝB��B�.B�pB�DB��B��B�?BĶB�MB�[B��B��B��B�B�dB�	B��B��B��B�TB��B��B��B��B��B� B~]BqABZQBM�BA�B2aB �BeB<BB�B�hB�B�-B��B�fB�tB��B��B�:B�mB�pBt�BgRBW�B=B(�B�BKB�lB�/B�B�B�pB�7BּBϑB�aB�FB��B�XB��By�Bo Ba-BZBRTBI�B;dB3�B*�B!�B�BjB
�B
�|B
�$B
ΊB
��B
ªB
�zB
�B
��B
�zB
��B
�+B
�BB
��B
��B
�RB
��B
�B
~BB
u�B
rB
pB
l�B
`BB
Y�B
T�B
Q�B
P.B
JrB
G�B
B[B
;B
2�B
-B
&�B
#�B
"B
 B
�B
qB
�B
�B

rB
1B
�B	��B	��B	��B	�rB	��B	��B	��B	��B	��B	��B	�3B	��B	�zB	��B	�SB	ӏB	��B	�xB	�B	��B	��B	��B	�'B	��B	��B	��B	��B	��B	��B	�_B	��B	�aB	�@B	� B	�VB	�lB	�GB	{�B	y�B	x�B	u�B	r�B	o�B	l�B	e`B	b�B	a�B	_�B	\�B	YeB	RB	N<B	J�B	HB	FYB	E9B	C�B	BuB	@�B	;�B	9XB	7�B	5�B	0�B	.IB	-CB	+QB	*B	'mB	%`B	!�B	!B	~B	�B	�B	sB	�B	�B	gB	gB	B	MB	�B	�B	�B	�B	�B	�B	�B	�B	�B	mB	GB	B	 4B�<B��B�B��B�tB�MB�vB��B��B�kB��B��B�B�B��B�B��BߊB�5B��B�)BڠB��B��B֡B�B�@B�B�oB��B�(B��B�~B̘B��B�xB��B�=B�rB�lB��B�1BɺB�KB�KB��B�zB�_B�+B��B�%B��BňB��B�B�B��BðB�gB��B��B�;B��B�'B��B��B�B�{B��B�MBżB�B��B��B�1B�_B�B�0BοBϫBЗB�NBңB�uB�FBյB��B�?B��B��B�]B��BޞB�!B�vB�ZB�B�B��B�B�qB��B�wB��B�}B�AB��B��B��B�LB�XB�HB	{B	SB	
�B	vB	4B	�B	�B	B	B	 \B	$�B	&fB	&B	'8B	-�B	0oB	2�B	<B	<B	<�B	<�B	=qB	=�B	@4B	AUB	B�B	C{B	D�B	F�B	IRB	JXB	K)B	K�B	L�B	QB	[#B	\�B	^�B	`�B	h
B	h>B	hXB	hsB	i�B	i�B	l"B	m)B	o�B	t�B	u�B	vFB	x8B	z�B	|6B	��B	��B	��B	��B	� B	��B	�kB	�IB	��B	��B	�B	�B	��B	��B	�B	��B	��B	��B	�B	��B	�B	�kB	��B	�B	�cB	��B	�3B	�FB	��B	��B	��B	��B	��B	��B	� B	��B	��B	�AB	�B	āB	ƎB	��B	ȴB	��B	̈́B	�BB	�.B	ԕB	�yB	ٚB	�B	��B	�hB	�B	�QB	�B	�WB	�/B	��B	�B	�MB	��B	��B	�}B
oB
GB
�B
1B
	�B

#B

rB

=B

�B
�B
�B
(B
NB
@B
2B
�B
B
�B
�B
WB
�B
B
CB
xB
xB
B
!B
# B
$&B
%`B
&B
'B
*�B
-]B
0UB
3B
6zB
7�B
9	B
9>B
;�B
?�B
AB
C�B
D�B
F�B
H�B
JXB
MB
N�B
NB
QB
R�B
SuB
T�B
U�B
VB
V�B
W�B
YB
[WB
\)B
]/B
_�B
bNB
cTB
c�B
d�B
e�B
gB
h$B
jB
lWB
mwB
m�B
m�B
p�B
qAB
q�B
q�B
r�B
u�B
vFB
vzB
v�B
w2B
wLB
wfB
x8B
x�B
x�B
y	B
z*B
|PB
|�B
~�B
}B
�B
��B
�AB
��B
��B
�B
�_B
�B
�	B
�rB
�DB
�0B
�"B
�bB
�B
��B
��B
��B
��B
��B
�+B
��B
��B
�	B
��B
�B
��B
��B
��B
�/B
�dB
�~B
�B
��B
�pB
��B
��B
�B
�tB
��B
��B
��B
��B
�`B
��B
�B
�mB
��B
�
B
�sB
��B
�B
�QB
��B
�qB
��B
�]B
�wB
��B
��B
��B
�}B
�5B
��B
��B
�B
��B
��B
�TB
�nB
��B
��B
��B
�LB
�RB
��B
��B
��B
�	B
�*B
��B
�0B
��B
�B
�PB
�"B
��B
��B
��B
��B
��B
��B
�AB
B
�GB
�{B
��B
�B
��B
��B
��B
�gB
��B
�B
�YB
�?B
��B
�%B
�B
�EB
�+B
��B
��B
�B
�B
��B
ʌB
��B
ˬB
�~B
̘B
��B
�B
�6B
�VB
�(B
�(B
��B
�NB
ѷB
�oB
�&B
��B
�MB
�B
�SB
�mB
�mB
�mB
��B
ؓB
�KB
�7B
�kB
ڠB
ڠB
�	B
�=B
ۦB
��B
��B
�)B
�CB
�/B
ݲB
�OB
��B
�;B
ߊB
�BB
��B
�B
�B
�B
��B
� B
��B
��B
�B
��B
��B
�B
�B
��B
��B
��B
�>B
��B
�_B
�KB
��B
�6B
�B
��B
��B
�=B
�=B
�"B
��B
�B
�qB
�WB
�WB
�WB
��B
��B
�/B
�B
�IB
�IB
�B
��B
�B
�B
�B
��B
��B
��B
��B
�B
��B
�B
�B
�vB
�B
��B
�aB
��B
�3B
�hB
�B
�B
��B
��B
�B
�+B
�`B
�zB
��B
�fB
��B
�lB
�XB
�XB
�rB
�^B
��B
�B
�B
�PB
��B
��B
�BB
�wB
�.B OB �B �B �B;B�B�B�B�B-B{BgB�BYB_B�B�B�BKB�B�B	B	lB	�B	�B	�B
XBDB^BxB�B0B�B�BVB�B�B�B�B�BBBvB�B�B�BBbB�B�B�B4B�B�B�B�B�B�B:BTBoBoB�B�B�B�B�B�B�B�B@B@BuBuB�B,BFB�B�B�B�B2BgB�B�BmBYB�BB+B�B�B�B�B�BB1BeBeBB�B�B�B�BQB	B#B=BWBqB�B�B�B�BBxB�B�B�B�B/B/BdB�BBjBjB�B�B�B!B;BpB�B BB �B �B!HB!�B!�B!�B"B"B"�B"�B"�B"�B#B# B#:B#nB#�B$ZB$tB$�B%B%B%,B%zB%�B%�B&B&LB&LB&fB&�B&�B&�B&�B&�B&�B&�B&�B'B'8B'�B'�B'�B(�B(�B)B)B)*B)yB)�B)�B*0B*�B*�B*B*�44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444B�B�nB�B�B�AB� B�mB��B�CB�dB�hB�,B�B�)B��B�B��B�TBΊB�fB��BѝB��B�.B�pB�DB��B��B�?BĶB�MB�[B��B��B��B�B�dB�	B��B��B��B�TB��B��B��B��B��B� B~]BqABZQBM�BA�B2aB �BeB<BB�B�hB�B�-B��B�fB�tB��B��B�:B�mB�pBt�BgRBW�B=B(�B�BKB�lB�/B�B�B�pB�7BּBϑB�aB�FB��B�XB��By�Bo Ba-BZBRTBI�B;dB3�B*�B!�B�BjB
�B
�|B
�$B
ΊB
��B
ªB
�zB
�B
��B
�zB
��B
�+B
�BB
��B
��B
�RB
��B
�B
~BB
u�B
rB
pB
l�B
`BB
Y�B
T�B
Q�B
P.B
JrB
G�B
B[B
;B
2�B
-B
&�B
#�B
"B
 B
�B
qB
�B
�B

rB
1B
�B	��B	��B	��B	�rB	��B	��B	��B	��B	��B	��B	�3B	��B	�zB	��B	�SB	ӏB	��B	�xB	�B	��B	��B	��B	�'B	��B	��B	��B	��B	��B	��B	�_B	��B	�aB	�@B	� B	�VB	�lB	�GB	{�B	y�B	x�B	u�B	r�B	o�B	l�B	e`B	b�B	a�B	_�B	\�B	YeB	RB	N<B	J�B	HB	FYB	E9B	C�B	BuB	@�B	;�B	9XB	7�B	5�B	0�B	.IB	-CB	+QB	*B	'mB	%`B	!�B	!B	~B	�B	�B	sB	�B	�B	gB	gB	B	MB	�B	�B	�B	�B	�B	�B	�B	�B	�B	mB	GB	B	 4B�<B��B�B��B�tB�MB�vB��B��B�kB��B��B�B�B��B�B��BߊB�5B��B�)BڠB��B��B֡B�B�@B�B�oB��B�(B��B�~B̘B��B�xB��B�=B�rB�lB��B�1BɺB�KB�KB��B�zB�_B�+B��B�%B��BňB��B�B�B��BðB�gB��B��B�;B��B�'B��B��B�B�{B��B�MBżB�B��B��B�1B�_B�B�0BοBϫBЗB�NBңB�uB�FBյB��B�?B��B��B�]B��BޞB�!B�vB�ZB�B�B��B�B�qB��B�wB��B�}B�AB��B��B��B�LB�XB�HB	{B	SB	
�B	vB	4B	�B	�B	B	B	 \B	$�B	&fB	&B	'8B	-�B	0oB	2�B	<B	<B	<�B	<�B	=qB	=�B	@4B	AUB	B�B	C{B	D�B	F�B	IRB	JXB	K)B	K�B	L�B	QB	[#B	\�B	^�B	`�B	h
B	h>B	hXB	hsB	i�B	i�B	l"B	m)B	o�B	t�B	u�B	vFB	x8B	z�B	|6B	��B	��B	��B	��B	� B	��B	�kB	�IB	��B	��B	�B	�B	��B	��B	�B	��B	��B	��B	�B	��B	�B	�kB	��B	�B	�cB	��B	�3B	�FB	��B	��B	��B	��B	��B	��B	� B	��B	��B	�AB	�B	āB	ƎB	��B	ȴB	��B	̈́B	�BB	�.B	ԕB	�yB	ٚB	�B	��B	�hB	�B	�QB	�B	�WB	�/B	��B	�B	�MB	��B	��B	�}B
oB
GB
�B
1B
	�B

#B

rB

=B

�B
�B
�B
(B
NB
@B
2B
�B
B
�B
�B
WB
�B
B
CB
xB
xB
B
!B
# B
$&B
%`B
&B
'B
*�B
-]B
0UB
3B
6zB
7�B
9	B
9>B
;�B
?�B
AB
C�B
D�B
F�B
H�B
JXB
MB
N�B
NB
QB
R�B
SuB
T�B
U�B
VB
V�B
W�B
YB
[WB
\)B
]/B
_�B
bNB
cTB
c�B
d�B
e�B
gB
h$B
jB
lWB
mwB
m�B
m�B
p�B
qAB
q�B
q�B
r�B
u�B
vFB
vzB
v�B
w2B
wLB
wfB
x8B
x�B
x�B
y	B
z*B
|PB
|�B
~�B
}B
�B
��B
�AB
��B
��B
�B
�_B
�B
�	B
�rB
�DB
�0B
�"B
�bB
�B
��B
��B
��B
��B
��B
�+B
��B
��B
�	B
��B
�B
��B
��B
��B
�/B
�dB
�~B
�B
��B
�pB
��B
��B
�B
�tB
��B
��B
��B
��B
�`B
��B
�B
�mB
��B
�
B
�sB
��B
�B
�QB
��B
�qB
��B
�]B
�wB
��B
��B
��B
�}B
�5B
��B
��B
�B
��B
��B
�TB
�nB
��B
��B
��B
�LB
�RB
��B
��B
��B
�	B
�*B
��B
�0B
��B
�B
�PB
�"B
��B
��B
��B
��B
��B
��B
�AB
B
�GB
�{B
��B
�B
��B
��B
��B
�gB
��B
�B
�YB
�?B
��B
�%B
�B
�EB
�+B
��B
��B
�B
�B
��B
ʌB
��B
ˬB
�~B
̘B
��B
�B
�6B
�VB
�(B
�(B
��B
�NB
ѷB
�oB
�&B
��B
�MB
�B
�SB
�mB
�mB
�mB
��B
ؓB
�KB
�7B
�kB
ڠB
ڠB
�	B
�=B
ۦB
��B
��B
�)B
�CB
�/B
ݲB
�OB
��B
�;B
ߊB
�BB
��B
�B
�B
�B
��B
� B
��B
��B
�B
��B
��B
�B
�B
��B
��B
��B
�>B
��B
�_B
�KB
��B
�6B
�B
��B
��B
�=B
�=B
�"B
��B
�B
�qB
�WB
�WB
�WB
��B
��B
�/B
�B
�IB
�IB
�B
��B
�B
�B
�B
��B
��B
��B
��B
�B
��B
�B
�B
�vB
�B
��B
�aB
��B
�3B
�hB
�B
�B
��B
��B
�B
�+B
�`B
�zB
��B
�fB
��B
�lB
�XB
�XB
�rB
�^B
��B
�B
�B
�PB
��B
��B
�BB
�wB
�.B OB �B �B �B;B�B�B�B�B-B{BgB�BYB_B�B�B�BKB�B�B	B	lB	�B	�B	�B
XBDB^BxB�B0B�B�BVB�B�B�B�B�BBBvB�B�B�BBbB�B�B�B4B�B�B�B�B�B�B:BTBoBoB�B�B�B�B�B�B�B�B@B@BuBuB�B,BFB�B�B�B�B2BgB�B�BmBYB�BB+B�B�B�B�B�BB1BeBeBB�B�B�B�BQB	B#B=BWBqB�B�B�B�BBxB�B�B�B�B/B/BdB�BBjBjB�B�B�B!B;BpB�B BB �B �B!HB!�B!�B!�B"B"B"�B"�B"�B"�B#B# B#:B#nB#�B$ZB$tB$�B%B%B%,B%zB%�B%�B&B&LB&LB&fB&�B&�B&�B&�B&�B&�B&�B&�B'B'8B'�B'�B'�B(�B(�B)B)B)*B)yB)�B)�B*0B*�B*�B*B*�44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220609124851  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220609124945  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220609124946  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220609124947                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220609214952  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220609214952  QCF$                G�O�G�O�G�O�            8000JA  ARUP                                                                        20220609130031                      G�O�G�O�G�O�                