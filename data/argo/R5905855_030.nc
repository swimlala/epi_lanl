CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T19:15:59Z creation;2022-06-04T19:16:00Z conversion to V3.1      
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
_FillValue                 �  I@   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M0   PRES_ADJUSTED_QC         
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
_FillValue                 �  �P   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �@   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    PSAL_ADJUSTED            
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
resolution        :�o     �  �`   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �    	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20220604191559  20220610151508  5905855                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               A   JA                                  2B  A   APEX                            8423                            2.11.2                          846 @��Ϥ��1   @��г�JV@.����m�c�?|�h1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @��@�  @�  A��A   A@  A^ffA�  A�  A�  A�  A�  A�  A�  A�  B ffBffB  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B���B���B�33B�  B�  B�ffB�ffB���B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C 33C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl�Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DEy�DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dx��Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D���D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D�3D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@=q@�Q�@�Q�AA (�A@(�A^�\A�{A�{A�{A�{A�{A�{A�{A�{B p�Bp�B
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
=B�B���B���B�8RB�B�B�k�B�k�B���B�B�B�B�B�B�B�B�B�B���B���B�B�B�B�B�B�B�B�B�B�B�B�C 5�C�C�C�C�C
�C�C�C�C�C�C�C�C�C�C�C �C"�C$�C&�C(�C*�C,�C.�C0�C2�C4�C6�C8�C:�C<�C>�C@�CB�CD�CF�CH�CJ�CL�CN�CP�CR�CT�CV�CX�CZ�C\�C^�C`�Cb�Cd�Cf�Ch�Cj�Cl)Cn�Cp�Cr�Ct�Cv�Cx�Cz�C|�C~�C�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�C�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HD  �D ��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D	 �D	��D
 �D
��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D  �D ��D! �D!��D" �D"��D# �D#��D$ �D$��D% �D%��D& �D&��D' �D'��D( �D(��D) �D)��D* �D*��D+ �D+��D, �D,��D- �D-��D. �D.��D/ �D/��D0 �D0��D1 �D1��D2 �D2��D3 �D3��D4 �D4��D5 �D5��D6 �D6��D7 �D7��D8 �D8��D9 �D9��D: �D:��D; �D;��D< �D<��D= �D=��D> �D>��D? �D?��D@ �D@��DA �DA��DB �DB��DC �DC��DD �DD��DE �DEz>DF �DF��DG �DG��DH �DH��DI �DI��DJ �DJ��DK �DK��DL �DL��DM �DM��DN �DN��DO �DO��DP �DP��DQ �DQ��DR �DR��DS �DS��DT �DT��DU �DU��DV �DV��DW �DW��DX �DX��DY �DY��DZ �DZ��D[ �D[��D\ �D\��D] �D]��D^ �D^��D_ �D_��D` �D`��Da �Da��Db �Db��Dc �Dc��Dd �Dd��De �De��Df �Df��Dg �Dg��Dh �Dh��Di �Di��Dj �Dj��Dk �Dk��Dl �Dl��Dm �Dm��Dn �Dn��Do �Do��Dp �Dp��Dq �Dq��Dr �Dr��Ds �Ds��Dt �Dt��Du �Du��Dv �Dv��Dw �Dw��Dx �Dx��Dx�>Dy��Dz �Dz��D{ �D{��D| �D|��D} �D}��D~ �D~��D �D��D� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�C�D��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�=D��RD��RD� RD�@RDRD��RD� RD�@RDÀRD��RD� RD�@RDĀRD��RD� RD�@RDŀRD��RD� RD�@RDƀRD��RD� RD�@RDǀRD��RD� RD�@RDȀRD��RD� RD�@RDɀRD��RD� RD�@RDʀRD��RD� RD�@RDˀRD��RD� RD�@RD̀RD��RD� RD�@RD̀RD��RD� RD�@RD΀RD��RD� RD�@RDπRD��RD� RD�@RDЀRD��RD��D�@RDрRD��RD� RD�@RDҀRD��RD� RD�@RDӀRD��RD� RD�@RDԀRD��RD� RD�@RDՀRD��RD� RD�@RDրRD��RD� RD�@RD׀RD��RD� RD�@RD؀RD��RD� RD�@RDـRD��RD� RD�@RDڀRD��RD� RD�@RDۀRD��RD� RD�@RD܀RD��RD� RD�@RD݀RD��RD� RD�@RDހRD��RD� RD�@RD߀RD��RD� RD�@RD��RD��RD� RD�@RD�RD��RD� RD�@RD⃅D��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD��RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��A��.A���A�;A� iA��A�YA�DA�hA�{A��A��A�A�A�A�A��A�VA� 'A� �A�!�A�!�A�"�A�#�A�%�A�&�A�&A�%zA�#nA��A��A��A��PAҔ�A�.A�n�A�d&A�[�A��A�&LA�c�A��vA���A�(XA�VA�K)A���A��!A�2-A�WsA�a|A���A���A��`A�,qA��LA���A�)_A��A��A�	lA��}A�B�A�O�A�R A���A���A��A���A��(A�~�A�<6A�p�A�_�A���A�ǮA���A�7�A�DA�2�A���A��A�q�A�y>A�e�A�ZA��A�A{�AwFAuV�At�jAt��As��ArAqoAo��Am$tAj�ZAg�>AgK�Af�Ab�5A_4nA\I�AV�,AS�AQs�AM�YAKo AIAC_A=�$A:bA7��A5�gA4��A4�A2��A1xA/��A.k�A-IRA,�TA,�\A+h�A)-wA(y�A&qA${�A"�'A!��A!��A!�A��A��A�A�A�&A]dA�vA��A#:A_�A4�A��A;�A4�A?�Ay>A|Ab�A{A��Ae�AI�A��A��AOA�A�7A��AYKA�nA�AHA�hA�Ao AH�A�0A�A�A��Ad�A4A�AX�A�.A҉A�TAa�A1�A
�A	A�A��A��A�QAl"As�A��AC�A�AVA�PA�A�AVA�gAg�A�-A�mA�A\�A$�A��A��A�FA�AL0A��AYKA�AA�PA �A 4@��@�v�@�{@��6@���@�&�@�q�@��'@��!@�_@�B�@�=q@��K@��@�{�@���@�o @�=@��F@�}�@��@�}V@��.@��a@�@�C�@��@��Y@�V@�P@�xl@�{@��/@�H@�5?@� �@쀝@��M@�o@�Q�@�m�@�+k@�:@�M�@��P@薼@��@��K@�`�@�$@�=�@�O@�i�@㯸@���@���@��@�9X@�c�@���@��@�RT@�1�@�E�@�hs@��@���@܋D@�c@��@ٷ�@��'@�K^@�˒@�l�@�.I@ִ9@�Ta@�5?@��@տH@Ռ~@�33@��@ԦL@��Q@�k�@��P@�Ov@�$@�RT@���@�r�@ϯ�@�_p@�/@�͟@��]@Γu@�1@͚k@�+@���@̅�@�G@�� @�o @�6z@�(�@�,=@�@Ȩ�@��@��@ǉ7@��@Ƙ_@�h�@��W@�1�@��/@Č�@�Ft@��>@�}�@�8�@�~(@�1'@���@��@�~(@��.@�K�@��@��9@�U2@���@�A�@�L�@�\)@�X@�2a@��v@�J@���@��@���@�B[@�x@��j@�S&@�"�@���@�^5@�$�@�O@���@��$@�{�@�7@���@�33@��}@�_@���@�@@���@��h@�Q�@�!@��*@���@���@�e�@��|@���@�e@�y�@�8@�@��@��|@���@��F@�ݘ@�n/@��@��f@��O@�1'@���@���@�a�@��@��2@� i@�ں@�~(@��z@���@�n/@�A�@�$t@���@���@��p@��@�v�@�H�@�!�@�b@��r@��[@�Vm@��@�c�@���@���@�Ɇ@��@�Xy@�M@��@�rG@�O@��E@��,@��@���@���@�5�@���@���@�_�@�M@��@��V@�c�@�7L@���@�w�@�<�@��@��]@��Z@���@�6z@���@�l�@��W@��@�{J@�:�@�!-@�bN@�#:@���@���@���@�W?@�:�@�Y@��@���@�M@�5?@��@���@�g�@�1�@�o@��@�֡@���@�8�@�
�@��P@��@��,@��@��@���@�w2@�A�@���@���@�y>@�H@��@��6@�x�@�Mj@��@�ߤ@�Ĝ@��@�q@�,=@���@�y�@�^�@�E9@��@���@���@��x@�z�@�!@��@���@�Y�@�@�҉@���@�U2@�1@�خ@��*@�j�@�`B@�"�@��z@�^5@�G@���@��:@��{@�2a@���@��\@�z@�\�@�x@��g@��@���@���@�X@� i@���@��}@���@��	@�F@�ߤ@�1�@��H@��V@�~�@�a�@�5�@�+@�/@�ߤ@��,@��@�͟@���@���@��-@���@�iD@�RT@�0�@�%F@��	@���@��@�c�@�@~��@~�@~4@}�#@}�9@}�^@}%@|��@|�@{�w@{4�@{�@z��@y�d@ys�@y0�@x��@x�D@xx@w��@w1�@v�B@v_�@v.�@u�)@u�X@uN<@t��@s˒@sS�@r�@r-@q�C@qO�@q�@p�p@p�@o�r@o�@nQ@n.�@n4@m�~@mIR@l��@l�o@l7�@k�Q@k��@ke�@k�@i�)@h�p@h,=@g��@g|�@g.I@f��@f�@fZ�@f�@e��@e��@eA @d�u@c�6@c�f@b��@bv�@bGE@a�@a��@`Ɇ@`:�@`�@_��@_.I@^u@]��@]`B@\��@\V�@[� @[Mj@Z�@Z�F@Z?@Z{@Z	@Y�@Y}�@X��@XM@Wn/@W@O@V�\@V=q@Uϫ@U�=@Up�@U%@T�@T�@TM@S�g@S��@S]�@Rߤ@R��@R:*@Q�M@Qa�@Q4@P��@P��@P4n@P1@O�@@O
=@Nz@N �@M�3@MJ�@L��@Ly>@LA�@K�@Kb�@J�8@J��@J��@J5?@I�)@Izx@I�@H��@Hc�@H%�@G�@G�@@G@O@G�@F�x@F�@E�d@E�@D�/@D`�@D�@Cƨ@Cs@B�2@B��@B�A@BV@B&�@A�^@Ae,@AQ�@A \@@�j@@�@?�@>�@>��@>?@>u@=��@=e,@=#�@<Ɇ@<�@<��@<e�@<1'@;�@;o@:ߤ@:�x@:�@9�N@9w2@9%F@8�9@8S�@7�g@7�0@7��@74�@7�@6�X@6�b@6p;@6_@5�C@5a�@52a@5�@4��@4~(@4"h@3�@3�V@3n/@3P�@3�@2�X@2��@2Ta@28�@2	@1�H@1Vm@1#�@0��@0�v@0Ɇ@0�u@0`�@0I�@0�@/��@/�@.�@.n�@.&�@-�C@-5�@,��@,��@,�@,l"@,	�@+��@+�0@+�@+b�@++@*�B@*��@*Ov@*$�@)��@)�n@)-w@(�|@(�$@(�D@(z�@(Z@(G@'iD@'
=@&�@&��@&\�@%�D@%�N@%�@%\�@%�@$�Y@$x@#�0@#y�@#U�@#6z@#+@#�@"�@"�6@"z@"?@"$�@" �@!�H@!w2@!+�@!+@!�@ ��@ m�@ Xy@ ?�@ x@��@�	@v`@_p@'�@�'@�x@Z�@!�@��@��@�S@/@�@�@D�@�r@��@W?@�M@�,@�@� @d�@R�@6�@!�@J@�@��@��@�S@L�@�@Ĝ@�e@�_@H@�@��@��@�P@U�@�s@�L@v�@M�@u@�@��@��@2a@Ĝ@��@��@oi@D�@�@�r@�
@��@�k@b�@'�@��@�'@��@i�@=q@&�@�@�@��@�"@:�@&�@�@��@ѷ@��@z�@Xy@4n@�r@��@��@��@�$@l�@1�@�@�@�}@�\@h
@-@u@��@�t@�"@Q�@!�@�f@�p@�@�@��@��@w�@e�@Q�@>B@$@7@�]@�6@��@�q@l�@;d@�@
҉@
�x@
c @
Q@
&�@
�@
 �@	�^@	zx@	k�@	8�@	!�@		l@�@�`@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��A��.A���A�;A� iA��A�YA�DA�hA�{A��A��A�A�A�A�A��A�VA� 'A� �A�!�A�!�A�"�A�#�A�%�A�&�A�&A�%zA�#nA��A��A��A��PAҔ�A�.A�n�A�d&A�[�A��A�&LA�c�A��vA���A�(XA�VA�K)A���A��!A�2-A�WsA�a|A���A���A��`A�,qA��LA���A�)_A��A��A�	lA��}A�B�A�O�A�R A���A���A��A���A��(A�~�A�<6A�p�A�_�A���A�ǮA���A�7�A�DA�2�A���A��A�q�A�y>A�e�A�ZA��A�A{�AwFAuV�At�jAt��As��ArAqoAo��Am$tAj�ZAg�>AgK�Af�Ab�5A_4nA\I�AV�,AS�AQs�AM�YAKo AIAC_A=�$A:bA7��A5�gA4��A4�A2��A1xA/��A.k�A-IRA,�TA,�\A+h�A)-wA(y�A&qA${�A"�'A!��A!��A!�A��A��A�A�A�&A]dA�vA��A#:A_�A4�A��A;�A4�A?�Ay>A|Ab�A{A��Ae�AI�A��A��AOA�A�7A��AYKA�nA�AHA�hA�Ao AH�A�0A�A�A��Ad�A4A�AX�A�.A҉A�TAa�A1�A
�A	A�A��A��A�QAl"As�A��AC�A�AVA�PA�A�AVA�gAg�A�-A�mA�A\�A$�A��A��A�FA�AL0A��AYKA�AA�PA �A 4@��@�v�@�{@��6@���@�&�@�q�@��'@��!@�_@�B�@�=q@��K@��@�{�@���@�o @�=@��F@�}�@��@�}V@��.@��a@�@�C�@��@��Y@�V@�P@�xl@�{@��/@�H@�5?@� �@쀝@��M@�o@�Q�@�m�@�+k@�:@�M�@��P@薼@��@��K@�`�@�$@�=�@�O@�i�@㯸@���@���@��@�9X@�c�@���@��@�RT@�1�@�E�@�hs@��@���@܋D@�c@��@ٷ�@��'@�K^@�˒@�l�@�.I@ִ9@�Ta@�5?@��@տH@Ռ~@�33@��@ԦL@��Q@�k�@��P@�Ov@�$@�RT@���@�r�@ϯ�@�_p@�/@�͟@��]@Γu@�1@͚k@�+@���@̅�@�G@�� @�o @�6z@�(�@�,=@�@Ȩ�@��@��@ǉ7@��@Ƙ_@�h�@��W@�1�@��/@Č�@�Ft@��>@�}�@�8�@�~(@�1'@���@��@�~(@��.@�K�@��@��9@�U2@���@�A�@�L�@�\)@�X@�2a@��v@�J@���@��@���@�B[@�x@��j@�S&@�"�@���@�^5@�$�@�O@���@��$@�{�@�7@���@�33@��}@�_@���@�@@���@��h@�Q�@�!@��*@���@���@�e�@��|@���@�e@�y�@�8@�@��@��|@���@��F@�ݘ@�n/@��@��f@��O@�1'@���@���@�a�@��@��2@� i@�ں@�~(@��z@���@�n/@�A�@�$t@���@���@��p@��@�v�@�H�@�!�@�b@��r@��[@�Vm@��@�c�@���@���@�Ɇ@��@�Xy@�M@��@�rG@�O@��E@��,@��@���@���@�5�@���@���@�_�@�M@��@��V@�c�@�7L@���@�w�@�<�@��@��]@��Z@���@�6z@���@�l�@��W@��@�{J@�:�@�!-@�bN@�#:@���@���@���@�W?@�:�@�Y@��@���@�M@�5?@��@���@�g�@�1�@�o@��@�֡@���@�8�@�
�@��P@��@��,@��@��@���@�w2@�A�@���@���@�y>@�H@��@��6@�x�@�Mj@��@�ߤ@�Ĝ@��@�q@�,=@���@�y�@�^�@�E9@��@���@���@��x@�z�@�!@��@���@�Y�@�@�҉@���@�U2@�1@�خ@��*@�j�@�`B@�"�@��z@�^5@�G@���@��:@��{@�2a@���@��\@�z@�\�@�x@��g@��@���@���@�X@� i@���@��}@���@��	@�F@�ߤ@�1�@��H@��V@�~�@�a�@�5�@�+@�/@�ߤ@��,@��@�͟@���@���@��-@���@�iD@�RT@�0�@�%F@��	@���@��@�c�@�@~��@~�@~4@}�#@}�9@}�^@}%@|��@|�@{�w@{4�@{�@z��@y�d@ys�@y0�@x��@x�D@xx@w��@w1�@v�B@v_�@v.�@u�)@u�X@uN<@t��@s˒@sS�@r�@r-@q�C@qO�@q�@p�p@p�@o�r@o�@nQ@n.�@n4@m�~@mIR@l��@l�o@l7�@k�Q@k��@ke�@k�@i�)@h�p@h,=@g��@g|�@g.I@f��@f�@fZ�@f�@e��@e��@eA @d�u@c�6@c�f@b��@bv�@bGE@a�@a��@`Ɇ@`:�@`�@_��@_.I@^u@]��@]`B@\��@\V�@[� @[Mj@Z�@Z�F@Z?@Z{@Z	@Y�@Y}�@X��@XM@Wn/@W@O@V�\@V=q@Uϫ@U�=@Up�@U%@T�@T�@TM@S�g@S��@S]�@Rߤ@R��@R:*@Q�M@Qa�@Q4@P��@P��@P4n@P1@O�@@O
=@Nz@N �@M�3@MJ�@L��@Ly>@LA�@K�@Kb�@J�8@J��@J��@J5?@I�)@Izx@I�@H��@Hc�@H%�@G�@G�@@G@O@G�@F�x@F�@E�d@E�@D�/@D`�@D�@Cƨ@Cs@B�2@B��@B�A@BV@B&�@A�^@Ae,@AQ�@A \@@�j@@�@?�@>�@>��@>?@>u@=��@=e,@=#�@<Ɇ@<�@<��@<e�@<1'@;�@;o@:ߤ@:�x@:�@9�N@9w2@9%F@8�9@8S�@7�g@7�0@7��@74�@7�@6�X@6�b@6p;@6_@5�C@5a�@52a@5�@4��@4~(@4"h@3�@3�V@3n/@3P�@3�@2�X@2��@2Ta@28�@2	@1�H@1Vm@1#�@0��@0�v@0Ɇ@0�u@0`�@0I�@0�@/��@/�@.�@.n�@.&�@-�C@-5�@,��@,��@,�@,l"@,	�@+��@+�0@+�@+b�@++@*�B@*��@*Ov@*$�@)��@)�n@)-w@(�|@(�$@(�D@(z�@(Z@(G@'iD@'
=@&�@&��@&\�@%�D@%�N@%�@%\�@%�@$�Y@$x@#�0@#y�@#U�@#6z@#+@#�@"�@"�6@"z@"?@"$�@" �@!�H@!w2@!+�@!+@!�@ ��@ m�@ Xy@ ?�@ x@��@�	@v`@_p@'�@�'@�x@Z�@!�@��@��@�S@/@�@�@D�@�r@��@W?@�M@�,@�@� @d�@R�@6�@!�@J@�@��@��@�S@L�@�@Ĝ@�e@�_@H@�@��@��@�P@U�@�s@�L@v�@M�@u@�@��@��@2a@Ĝ@��@��@oi@D�@�@�r@�
@��@�k@b�@'�@��@�'@��@i�@=q@&�@�@�@��@�"@:�@&�@�@��@ѷ@��@z�@Xy@4n@�r@��@��@��@�$@l�@1�@�@�@�}@�\@h
@-@u@��@�t@�"@Q�@!�@�f@�p@�@�@��@��@w�@e�@Q�@>B@$@7@�]@�6@��@�q@l�@;d@�@
҉@
�x@
c @
Q@
&�@
�@
 �@	�^@	zx@	k�@	8�@	!�@		l@�@�`@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B	��B	��B	��B	��B	�B	�B	�B	�3B	�B	��B	��B	�B	��B	�B	��B	�B	��B	��B	��B	��B	�BB
  B
 �B
 B
'B
uB
�B
�B
�B
�B
�B
'B	�cB
	RB

rB
bB
C-B
�$B
��B
�=B
ȴB
��B
�~B
�B
��B
��BB�B�BBB2�BA;B��B�B�MB�)B�CBuB�.B�B��B�TB�*B��B��B�aBq�B]�B)DB5B1B �B�B
�B
�B
�8B
�@B
�)B
οB
�XB
�?B
�'B
�?B
��B
��B
T,B
?�B
'�B
4B
�B
 4B	��B	��B	�AB	��B	ܬB	ЗB	��B	��B	��B	��B	�B	r�B	^�B	5�B	�B	uB��B�+B�B�XB��B��B�-B�|B��B��B��B��B�'B�B�nB�>B��B�BB��B�tBňB��B��B�lB��B�+B��B�+B��B�cB�?BڠB�;B�VB� B�B�B��B	1B	FB	W�B	cB	}�B	�B	�_B	�WB	�NB	�$B	��B	��B	�jB	��B	ΥB	��B	��B	�3B	�cB	�_B	�0B	��B	��B	˒B	��B	�GB	�iB	B	��B	��B	��B	��B	�NB	��B	��B	��B	��B	��B	y>B	T{B	W$B	J	B	0�B	4B	WYB	\�B	N�B	NB	_;B	k6B	raB	�B	� B	�bB	�	B	�0B	�BB	�EB	�B	��B	��B	��B	��B	��B	��B	� B	�B	��B	�cB	�BB	��B	�UB	�MB	ŢB	��B	�+B	ǔB	ǮB	�fB	ǔB	�_B	�B	ňB	ƨB	�1B	�EB	ƨB	�YB	�EB	��B	��B	�_B	ǮB	��B	��B	�B	��B	�rB	��B	̘B	̳B	��B	�JB	�B	�(B	�NB	�mB	�/B	�/B	��B	��B	��B	�;B	��B	�mB	֡B	ٚB	�yB	��B	ևB	�EB	��B	�=B	�7B	یB	�kB	޸B	�`B	�tB	�B	��B	�NB	��B	�&B	�B	�B	�B	�,B	�B	��B	�B	�LB	��B	��B	�B	��B	��B	�_B	��B	��B	��B	��B	�B	�B	�iB	�B	�B	�5B	�IB	�oB	��B	�'B	��B	�|B	�B	��B	�'B	�B	�nB	��B	��B	��B	�nB	��B	�TB	�9B	�TB	�B	�B	�GB	�B	�B	�aB	�B	��B	��B	��B	�B	�B	�ZB	��B	�B	��B	��B	�zB	�B	��B	�B	��B	�MB	��B	�B	�]B	��B	�)B	�}B	�OB	�IB	��B	�!B	�aB	��B	�B	�B	�B	��B	�aB	�B	��B	��B	�B	�TB	�B	��B	��B	��B	��B	�nB	��B	��B	��B	�fB	��B	��B	��B	�>B	��B	��B	�	B	��B	��B	�^B	��B	��B	��B	��B	��B	�xB	�*B	��B	�dB	��B	��B	��B	�qB	��B	��B	�(B	��B
 �B
 �B
 4B	��B	��B	��B
'B
MB
�B
B
mB
�B
�B
B
?B
%B
tB
�B
_B
�B
�B
�B
�B
B
�B
�B
EB
tB
�B
�B
�B
�B
B
mB
�B
�B
B
tB

	B
	�B
	7B
�B
B
�B
KB
�B
	7B

�B

�B

�B
B

�B

�B
�B
�B
�B
~B
B
�B
^B
xB
DB
�B
�B
�B
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
B
�B
�B
B
BB
�B
�B
B
NB
B
:B
�B
B
B
B
B
�B
�B
�B
aB
�B
�B
aB
aB
aB
�B
2B
2B
gB
�B
9B
�B
�B
YB
B
+B
+B
EB
B
1B
1B
KB
KB
B
�B
QB
	B
WB
�B
CB
]B
�B
�B
B
�B
dB
�B
�B
�B
VB
pB
�B
�B
 BB
�B
�B
VB
pB
�B
 BB
 \B
 vB
 BB
 �B
 �B
 �B
 \B
!�B
!B
 �B
!HB
!bB
 �B
 'B
 BB
 B
 vB
 �B
!�B
"�B
#B
#:B
$�B
%�B
$�B
$�B
$�B
&�B
'8B
'B
'RB
'�B
'�B
'�B
'�B
(XB
)yB
*KB
+kB
,=B
,=B
,qB
-CB
-]B
-)B
-B
-B
.B
.�B
/�B
/�B
/�B
/iB
/B
/�B
0;B
0oB
0�B
0�B
0�B
1'B
1�B
2�B
3hB
3�B
4TB
4�B
4�B
4�B
5%B
5ZB
5tB
5�B
5�B
6FB
6�B
6�B
7LB
88B
8�B
9$B
9$B
9XB
:*B
:�B
:�B
:DB
:�B
:DB
:B
:*B
:^B
:�B
:�B
:�B
;B
<B
<�B
=<B
=<B
=�B
>wB
>�B
?B
?cB
?cB
?�B
@ B
@iB
@4B
?�B
@ B
@4B
@4B
@B
@iB
AB
AUB
A�B
B[B
CB
B�B
C-B
C�B
DMB
D�B
EB
E�B
E�B
E�B
F%B
FYB
F?B
F�B
GEB
G+B
G_B
GEB
G�B
G�B
HB
H1B
H1B
H�B
H�B
IB
J	B
I�B
J=B
J�B
J�B
K)B
KDB
K�B
K�B
KDB
K)B
KDB
K�B
L~B
LdB
LJB
LdB
L�B
MB
M�B
N�B
OBB
O\B
O�B
O�B
PHB
PHB
P}B
P}B
P�B
QB
Q B
Q4B
Q�B
Q�B
Q�B
RoB
R�B
R�B
R�B
S&B
S[B
S�B
S�B
TB
TB
T{B
T�B
T�B
T�B
T�B
U2B
UgB
U�B
U�B
VB
VB
V9B
V�B
V�B
V�B
W
B
W
B
W
B
W
B
WsB
W�B
W�B
XB
X_B
XyB
X�B
X�B
YKB
Y�B
ZB
ZB
Z7B
ZkB
ZQB
Z�B
Z�B
Z�B
[#B
[=B
[�B
[�B
[�B
[�B
\B
\]B
\]B
\�B
\�B
\�B
\�B
]/B
]dB
]~B
]~B
]�B
]�B
^B
^jB
^�B
^�B
^�B
^�B
^�B
^�B
^�B
_!B
_�B
_�B
_�B
`'B
`�B
`�B
aB
a-B
aHB
abB
a�B
a�B
a�B
bB
bB
b4B
b�B
b�B
cB
cB
c:B
cnB
c�B
dB
d@B
dtB
dtB
dtB
d�B
e`B
e�B
e�B
e�B
fB
f�B
f�B
f�B
gB
g8B
g�B
h$B
hXB
h�B
h�B
h�B
h�B
h�B
iB
i*B
iDB
i�B
i�B
i�B
i�B
j0B
jB
jeB
jB
j�B
kB
kB
kB
k6B
kkB
k�B
k�B
k�B
k�B
l=B
lWB
l�B
l�B
l�B
l�B
mCB
mwB
m�B
m�B
nIB
ncB
n�B
o B
o�B
oiB
oiB
o�B
o�B
o�B
pB
pB
p!B
p;B
p;B
pUB
pUB
p�B
q'B
qvB
qvB
q�B
q�B
q�B
rB
rGB
rGB
r|B
r�B
sB
sMB
shB
s�B
s�B
s�B
tB
tTB
t�B
t�B
t�B
uB
u%B
uZB
u�B
u�B
u�B
u�B
u�B
v+B
vFB
vzB
v�B
v�B
v�B
wB
v�B
wfB
w�B
w�B
w�B
xB
x8B
x8B
xRB
xlB
x�B
x�B
x�B
y	B
y>B
y>B
y�B
yrB
y�B
y�B
y�B
zB
zDB
z^B
z�B
z�B
z�B
{B
{dB
{�B
{�B
|B
|PB
|jB
|�B
|�B
|�B
|�B
|�B
|�B
}B
}"B
}<B
}"B
}<B
}qB
}qB
}qB
}�B
}�B
}�B
~B
~(B
~(B
~BB
~wB
~�B
~�B
~�B
.B
.B
�B
�B
�B
�B
�B
�4111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B	��B	��B	��B	��B	�B	�B	�B	�3B	�B	��B	��B	�B	��B	�B	��B	�B	��B	��B	��B	��B	�BB
  B
 �B
 B
'B
uB
�B
�B
�B
�B
�B
'B	�cB
	RB

rB
bB
C-B
�$B
��B
�=B
ȴB
��B
�~B
�B
��B
��BB�B�BBB2�BA;B��B�B�MB�)B�CBuB�.B�B��B�TB�*B��B��B�aBq�B]�B)DB5B1B �B�B
�B
�B
�8B
�@B
�)B
οB
�XB
�?B
�'B
�?B
��B
��B
T,B
?�B
'�B
4B
�B
 4B	��B	��B	�AB	��B	ܬB	ЗB	��B	��B	��B	��B	�B	r�B	^�B	5�B	�B	uB��B�+B�B�XB��B��B�-B�|B��B��B��B��B�'B�B�nB�>B��B�BB��B�tBňB��B��B�lB��B�+B��B�+B��B�cB�?BڠB�;B�VB� B�B�B��B	1B	FB	W�B	cB	}�B	�B	�_B	�WB	�NB	�$B	��B	��B	�jB	��B	ΥB	��B	��B	�3B	�cB	�_B	�0B	��B	��B	˒B	��B	�GB	�iB	B	��B	��B	��B	��B	�NB	��B	��B	��B	��B	��B	y>B	T{B	W$B	J	B	0�B	4B	WYB	\�B	N�B	NB	_;B	k6B	raB	�B	� B	�bB	�	B	�0B	�BB	�EB	�B	��B	��B	��B	��B	��B	��B	� B	�B	��B	�cB	�BB	��B	�UB	�MB	ŢB	��B	�+B	ǔB	ǮB	�fB	ǔB	�_B	�B	ňB	ƨB	�1B	�EB	ƨB	�YB	�EB	��B	��B	�_B	ǮB	��B	��B	�B	��B	�rB	��B	̘B	̳B	��B	�JB	�B	�(B	�NB	�mB	�/B	�/B	��B	��B	��B	�;B	��B	�mB	֡B	ٚB	�yB	��B	ևB	�EB	��B	�=B	�7B	یB	�kB	޸B	�`B	�tB	�B	��B	�NB	��B	�&B	�B	�B	�B	�,B	�B	��B	�B	�LB	��B	��B	�B	��B	��B	�_B	��B	��B	��B	��B	�B	�B	�iB	�B	�B	�5B	�IB	�oB	��B	�'B	��B	�|B	�B	��B	�'B	�B	�nB	��B	��B	��B	�nB	��B	�TB	�9B	�TB	�B	�B	�GB	�B	�B	�aB	�B	��B	��B	��B	�B	�B	�ZB	��B	�B	��B	��B	�zB	�B	��B	�B	��B	�MB	��B	�B	�]B	��B	�)B	�}B	�OB	�IB	��B	�!B	�aB	��B	�B	�B	�B	��B	�aB	�B	��B	��B	�B	�TB	�B	��B	��B	��B	��B	�nB	��B	��B	��B	�fB	��B	��B	��B	�>B	��B	��B	�	B	��B	��B	�^B	��B	��B	��B	��B	��B	�xB	�*B	��B	�dB	��B	��B	��B	�qB	��B	��B	�(B	��B
 �B
 �B
 4B	��B	��B	��B
'B
MB
�B
B
mB
�B
�B
B
?B
%B
tB
�B
_B
�B
�B
�B
�B
B
�B
�B
EB
tB
�B
�B
�B
�B
B
mB
�B
�B
B
tB

	B
	�B
	7B
�B
B
�B
KB
�B
	7B

�B

�B

�B
B

�B

�B
�B
�B
�B
~B
B
�B
^B
xB
DB
�B
�B
�B
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
B
�B
�B
B
BB
�B
�B
B
NB
B
:B
�B
B
B
B
B
�B
�B
�B
aB
�B
�B
aB
aB
aB
�B
2B
2B
gB
�B
9B
�B
�B
YB
B
+B
+B
EB
B
1B
1B
KB
KB
B
�B
QB
	B
WB
�B
CB
]B
�B
�B
B
�B
dB
�B
�B
�B
VB
pB
�B
�B
 BB
�B
�B
VB
pB
�B
 BB
 \B
 vB
 BB
 �B
 �B
 �B
 \B
!�B
!B
 �B
!HB
!bB
 �B
 'B
 BB
 B
 vB
 �B
!�B
"�B
#B
#:B
$�B
%�B
$�B
$�B
$�B
&�B
'8B
'B
'RB
'�B
'�B
'�B
'�B
(XB
)yB
*KB
+kB
,=B
,=B
,qB
-CB
-]B
-)B
-B
-B
.B
.�B
/�B
/�B
/�B
/iB
/B
/�B
0;B
0oB
0�B
0�B
0�B
1'B
1�B
2�B
3hB
3�B
4TB
4�B
4�B
4�B
5%B
5ZB
5tB
5�B
5�B
6FB
6�B
6�B
7LB
88B
8�B
9$B
9$B
9XB
:*B
:�B
:�B
:DB
:�B
:DB
:B
:*B
:^B
:�B
:�B
:�B
;B
<B
<�B
=<B
=<B
=�B
>wB
>�B
?B
?cB
?cB
?�B
@ B
@iB
@4B
?�B
@ B
@4B
@4B
@B
@iB
AB
AUB
A�B
B[B
CB
B�B
C-B
C�B
DMB
D�B
EB
E�B
E�B
E�B
F%B
FYB
F?B
F�B
GEB
G+B
G_B
GEB
G�B
G�B
HB
H1B
H1B
H�B
H�B
IB
J	B
I�B
J=B
J�B
J�B
K)B
KDB
K�B
K�B
KDB
K)B
KDB
K�B
L~B
LdB
LJB
LdB
L�B
MB
M�B
N�B
OBB
O\B
O�B
O�B
PHB
PHB
P}B
P}B
P�B
QB
Q B
Q4B
Q�B
Q�B
Q�B
RoB
R�B
R�B
R�B
S&B
S[B
S�B
S�B
TB
TB
T{B
T�B
T�B
T�B
T�B
U2B
UgB
U�B
U�B
VB
VB
V9B
V�B
V�B
V�B
W
B
W
B
W
B
W
B
WsB
W�B
W�B
XB
X_B
XyB
X�B
X�B
YKB
Y�B
ZB
ZB
Z7B
ZkB
ZQB
Z�B
Z�B
Z�B
[#B
[=B
[�B
[�B
[�B
[�B
\B
\]B
\]B
\�B
\�B
\�B
\�B
]/B
]dB
]~B
]~B
]�B
]�B
^B
^jB
^�B
^�B
^�B
^�B
^�B
^�B
^�B
_!B
_�B
_�B
_�B
`'B
`�B
`�B
aB
a-B
aHB
abB
a�B
a�B
a�B
bB
bB
b4B
b�B
b�B
cB
cB
c:B
cnB
c�B
dB
d@B
dtB
dtB
dtB
d�B
e`B
e�B
e�B
e�B
fB
f�B
f�B
f�B
gB
g8B
g�B
h$B
hXB
h�B
h�B
h�B
h�B
h�B
iB
i*B
iDB
i�B
i�B
i�B
i�B
j0B
jB
jeB
jB
j�B
kB
kB
kB
k6B
kkB
k�B
k�B
k�B
k�B
l=B
lWB
l�B
l�B
l�B
l�B
mCB
mwB
m�B
m�B
nIB
ncB
n�B
o B
o�B
oiB
oiB
o�B
o�B
o�B
pB
pB
p!B
p;B
p;B
pUB
pUB
p�B
q'B
qvB
qvB
q�B
q�B
q�B
rB
rGB
rGB
r|B
r�B
sB
sMB
shB
s�B
s�B
s�B
tB
tTB
t�B
t�B
t�B
uB
u%B
uZB
u�B
u�B
u�B
u�B
u�B
v+B
vFB
vzB
v�B
v�B
v�B
wB
v�B
wfB
w�B
w�B
w�B
xB
x8B
x8B
xRB
xlB
x�B
x�B
x�B
y	B
y>B
y>B
y�B
yrB
y�B
y�B
y�B
zB
zDB
z^B
z�B
z�B
z�B
{B
{dB
{�B
{�B
|B
|PB
|jB
|�B
|�B
|�B
|�B
|�B
|�B
}B
}"B
}<B
}"B
}<B
}qB
}qB
}qB
}�B
}�B
}�B
~B
~(B
~(B
~BB
~wB
~�B
~�B
~�B
.B
.B
�B
�B
�B
�B
�B
�4111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604105232  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604191559  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604191600  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604191600                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605041607  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605041607  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610151508                      G�O�G�O�G�O�                