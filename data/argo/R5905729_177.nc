CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2023-03-02T10:01:41Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
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
resolution        >�EȠ      
_FillValue        A.�~       axis      T           80   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    88   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
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
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  `�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  p    TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �H   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �0   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �p   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �X   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ݀   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ݰ   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �,   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �<   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �@   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �P   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �T   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �X   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �\Argo profile    3.1 1.2 19500101000000  20230302100141  20230302100141  5905729 Argo PMEL                                                       GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               �A   AO  7160                            2B  A   NAVIS_A                         0838                            170425                          863 @��ng_1   @��1M��@*�$�/�d��\)1   GPS     Primary sampling: averaged                                                                                                                                                                                                                                         �A   A   A   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�33B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C�C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C+�fC-�fC0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CG�fCJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$�fD%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8y�D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ Dͼ�D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�0 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@�Q�A (�A (�A@(�A`(�A�{A�{A�{A�{A�{A�{A�{A�G�B 
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
=B�B�B���B�B�B�B�B�B�B�B�8RB�B�B�B���B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�C �C�C�C�C�C
�C�C)C�C�C�C�C�C�C�C�C �C"�C$�C&�C(�C*�C+��C-��C0�C2�C4�C6�C8�C:�C<�C>�C@�CB�CD�CF�CG��CJ�CL�CN�CP�CR�CT�CV�CX�CZ�C\�C^�C`�Cb�Cd�Cf�Ch�Cj�Cl�Cn�Cp�Cr�Ct�Cv�Cx�Cz�C|�C~�C�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC��{C�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC��{C�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HD  �D ��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D	 �D	��D
 �D
��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D  �D ��D! �D!��D" �D"��D# �D#��D$ �D$�
D% �D%��D& �D&��D' �D'��D( �D(��D) �D)��D* �D*��D+ �D+��D, �D,��D- �D-��D. �D.��D/ �D/��D0 �D0��D1 �D1��D2 �D2��D3 �D3��D4 �D4��D5 �D5��D6 �D6��D7 �D7��D8 �D8z>D9 �D9��D: �D:��D; �D;��D< �D<��D= �D=��D> �D>��D? �D?��D@ �D@��DA �DA��DB �DB��DC �DC��DD �DD��DE �DE��DF �DF��DG �DG��DH �DH��DI �DI��DJ �DJ��DK �DK��DL �DL��DM �DM��DN �DN��DO �DO��DP �DP��DQ �DQ��DR �DR��DS �DS��DT �DT��DU �DU��DV �DV��DW �DW��DX �DX��DY �DY��DZ �DZ��D[ �D[��D\ �D\��D] �D]��D^ �D^��D_ �D_��D` �D`��Da �Da��Db �Db��Dc �Dc��Dd �Dd��De �De��Df �Df��Dg �Dg��Dh �Dh��Di �Di��Dj �Dj��Dk �Dk��Dl �Dl��Dm �Dm��Dn �Dn��Do �Do��Dp �Dp��Dq �Dq��Dr �Dr��Ds �Ds��Dt �Dt��Du �Du��Dv �Dv��Dw �Dw��Dx �Dx��Dy �Dy��Dz �Dz��D{ �D{��D| �D|��D} �D}��D~ �D~��D �D��D� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��D� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RDRD��RD� RD�@RDÀRD��RD� RD�@RDĀRD��RD� RD�@RDŀRD��RD� RD�@RDƀRD��RD� RD�@RDǀRD��RD� RD�@RDȀRD��RD� RD�@RDɀRD��RD� RD�@RDʀRD��RD� RD�@RDˀRD��RD� RD�@RD̀RD��RD� RD�@RD̀RDͽD� RD�@RD΀RD��RD� RD�@RDπRD��RD� RD�@RDЀRD��RD� RD�@RDрRD��RD� RD�@RDҀRD��RD� RD�@RDӀRD��RD� RD�@RDԀRD��RD� RD�@RDՀRD��RD� RD�@RDրRD��RD� RD�@RD׀RD��RD� RD�@RD؀RD��RD� RD�@RDـRD��RD� RD�@RDڀRD��RD� RD�@RDۀRD��RD� RD�@RD܀RD��RD� RD�@RD݀RD��RD� RD�@RDހRD��RD� RD�@RD߀RD��RD� RD�@RD��RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD��RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD��D�0R1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��HA��TA��TA��HA��HA��TA��yA��A��A��A��A��A��A��A���A���A���A���A���A���A��A��A��A���A���A���A���A���A���A���A���A���A���A���A���A��A��`AѴ9Aя\A���A�\)A�bA͍PA�/A�`BA�$�A�hsA�A�A�;dA�-A�+A��;A���A�^5A��/A���A�oA��uA���A�hsA�-A��A�~�A���A�x�A�`BA�C�A�A�A��PA��wA�ZA�%A�^5A��A�ZA�\)A���A�G�A~(�A|��Az��AvbAsoAo��AiK�Agl�Af��Ae��Ad�!Aa�A\M�AW�AU��AR��AO�
AN�AJ��AC+A?�^A=oA;��A:Q�A9O�A9?}A9oA8ZA6Q�A6  A5�-A4ĜA4$�A2��A1�A/l�A.��A/33A//A.�`A.(�A,M�A+��A*�A*�\A)��A)�A)C�A)�A(�uA((�A'x�A&�A&-A%�^A%�^A%�;A%�^A%x�A%/A$�A$E�A#�-A#O�A"��A"��A"E�A!�mA!|�A!C�A �yA ffA 5?A�wA�A�HA�9A�A�7A%AJA��A��A/A�HA�+A=qAƨAoAȴAffA^5A1At�A`BA`BA&�A��A��Av�Ax�A�jAVAdZAA�\AE�A �A��A��A-A��AdZAK�A"�A��A5?A�^A`BA/AĜAz�A=qAM�A5?A{A�A
��A
1'A
bA	��A
  A	�A	A	hsA��AM�A�#A=qA1A��A�A?}A�A-AdZA+A��A��AffAv�A=qA�A�A�A�AA n�A {@��;@�$�@�Ĝ@�t�@�/@��@��`@��@�7L@�/@��/@��@�5?@���@� �@�C�@�ȴ@�z�@�t�@�\)@�ȴ@�G�@�9X@��;@�\)@��@�$�@�w@�@�\@�E�@�7@�?}@�%@�Ĝ@�D@�1@�+@�7L@�j@�z�@�9X@ߕ�@���@�J@�p�@�7L@��/@�Z@��;@ۍP@�\)@�@١�@�Ĝ@�Z@�1@���@�|�@�"�@�$�@թ�@ԣ�@�Q�@�1@�\)@�o@��@�ff@�?}@��`@д9@�z�@�1@��
@���@�dZ@�
=@���@�n�@�Ĝ@��@˅@��y@�5?@ə�@�p�@ȴ9@ǝ�@��@�V@���@�p�@��@�j@�1@öF@�;d@�"�@��y@�~�@�J@���@��^@��7@�hs@���@�9X@��@�ƨ@��w@��F@�t�@�+@��H@�E�@�5?@��@�@��@��@��T@�X@��j@�I�@���@�dZ@�o@���@�ff@��7@��@�dZ@�@���@�ȴ@�-@�V@��D@�I�@��m@��P@�@��R@�^5@�=q@�@��7@�Ĝ@�j@��
@�|�@��@�
=@��@�ff@�-@�J@�@���@���@��h@�x�@�p�@�`B@�G�@�?}@�&�@�V@���@��@��m@�;d@��@���@���@���@��\@�=q@��j@�1'@��@�1@��@�dZ@�;d@�33@���@�5?@�@��7@�V@�r�@� �@�1@���@�o@�{@��-@��7@�G�@��u@�(�@���@�K�@�33@��y@�=q@���@�G�@�&�@���@���@��9@���@��u@�z�@�j@�bN@�Z@�1@�l�@�@�n�@�=q@�5?@�$�@���@��7@��@�1@�t�@�C�@�;d@�;d@��@��!@�$�@��T@���@��7@�X@�&�@�Ĝ@���@�A�@��;@�l�@�S�@�33@�ȴ@�V@�@�p�@�X@���@�j@�9X@��@�|�@�33@���@��!@��\@�M�@�@��@�X@�%@���@�bN@�b@�ƨ@���@��P@��@�S�@�
=@���@��@��@��!@���@���@�E�@��@�@��T@���@�O�@���@���@�bN@�I�@�A�@�9X@�(�@�1@��@��w@��@�\)@�33@��@���@�~�@�-@���@���@���@��j@�r�@�bN@�Z@��;@�dZ@�+@�@��y@��@�M�@��T@��-@�`B@��@�V@�%@���@��`@���@�Ĝ@��9@��9@���@��@�Z@�9X@��@�P@~ȴ@~v�@}�T@}�@}/@|�D@{�
@{��@{�@{t�@{dZ@z��@z^5@y�#@xĜ@xbN@xb@w�;@w+@v{@up�@uV@t�/@t��@t��@t�D@tz�@tj@tZ@s��@sdZ@r�!@rM�@q��@qhs@q%@p �@o\)@o
=@nV@m�h@m`B@mV@l9X@j�@jn�@j^5@j=q@i��@i��@ihs@h�`@hA�@g�;@g\)@g;d@g
=@fV@e�-@e/@d��@c��@cdZ@b�H@b-@a�7@a&�@a�@a%@`��@`Ĝ@`��@`r�@`bN@` �@_��@_|�@^�y@^��@^5?@]�T@]��@]@]��@]p�@]?}@\�@\�/@\Z@\�@[�m@[�F@[@Z=q@Y��@Y��@Y�7@YG�@Y�@X�`@X��@X�@Xb@W�P@WK�@V�y@V�+@VE�@V@U@U/@T�j@Tj@S��@S�
@S�F@R��@Qhs@Q�@PĜ@P�@PbN@PA�@O�@O�@O�P@N��@N@M�-@M?}@L�@LZ@L�@K�m@K��@KC�@Ko@J��@J�\@JM�@J�@J�@JJ@I��@I&�@H�u@H �@G��@Gl�@G�@F�R@F@E��@E�-@E�h@E/@D�/@D�D@D�D@Dj@DI�@D�@D1@Cƨ@CC�@B�H@B�\@B=q@A�#@A�^@A�^@A�7@Ax�@AX@A7L@@��@@bN@?�@?\)@?�@>�R@=p�@<�/@<�@<�D@<I�@:�@:��@:�!@:�!@:�\@:J@9�7@8��@8r�@8bN@8A�@8b@7��@7\)@6��@6E�@5�@5�T@5@5��@5p�@5O�@5/@5�@5�@4�@4I�@3�m@3��@3o@2~�@1��@1��@1��@1hs@1&�@0�@/��@/K�@/
=@.�y@.ȴ@.ff@-�-@-��@-p�@-O�@-O�@-?}@-/@,�j@,(�@+��@+ƨ@+dZ@*��@*-@*J@)�@)�^@)hs@)%@(�u@(�@(r�@(Q�@(  @'��@'�P@'\)@';d@'�@&�y@&�R@&v�@&E�@&5?@&5?@&$�@&$�@&{@&@%�T@%��@%@%��@%p�@%O�@%/@$��@$�@$I�@#��@#dZ@#"�@"��@"�!@"��@"^5@"-@"�@"J@!��@!��@!�@!��@!x�@!&�@ �`@ ��@ ��@ ��@ �`@ �`@ �`@ Ĝ@ ��@ �u@ �u@ �u@ �u@ �u@ r�@  �@   @�@�@�@�w@l�@
=@�@ȴ@�R@��@�+@ff@V@5?@@@��@?}@V@�@z�@j@I�@9X@��@�F@��@t�@S�@C�@"�@�H@~�@^5@�@�@�#@��@x�@7L@%@��@�`@Ĝ@Ĝ@Q�@K�@+@�@v�@v�@ff@E�@`B@V@��@��@z�@I�@�@ƨ@��@��@��@��@t�@S�@33@o@@�H@��@~�@��@�@��@G�@�@��@��@Ĝ@�9@�u@b@�@|�@\)@�@��@v�@ff@@�@?}@�@��@j@I�@(�@1@�m@ƨ@��@��@��@t�@C�@33@"�@o@o@@1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��HA��TA��TA��HA��HA��TA��yA��A��A��A��A��A��A��A���A���A���A���A���A���A��A��A��A���A���A���A���A���A���A���A���A���A���A���A���A��A��`AѴ9Aя\A���A�\)A�bA͍PA�/A�`BA�$�A�hsA�A�A�;dA�-A�+A��;A���A�^5A��/A���A�oA��uA���A�hsA�-A��A�~�A���A�x�A�`BA�C�A�A�A��PA��wA�ZA�%A�^5A��A�ZA�\)A���A�G�A~(�A|��Az��AvbAsoAo��AiK�Agl�Af��Ae��Ad�!Aa�A\M�AW�AU��AR��AO�
AN�AJ��AC+A?�^A=oA;��A:Q�A9O�A9?}A9oA8ZA6Q�A6  A5�-A4ĜA4$�A2��A1�A/l�A.��A/33A//A.�`A.(�A,M�A+��A*�A*�\A)��A)�A)C�A)�A(�uA((�A'x�A&�A&-A%�^A%�^A%�;A%�^A%x�A%/A$�A$E�A#�-A#O�A"��A"��A"E�A!�mA!|�A!C�A �yA ffA 5?A�wA�A�HA�9A�A�7A%AJA��A��A/A�HA�+A=qAƨAoAȴAffA^5A1At�A`BA`BA&�A��A��Av�Ax�A�jAVAdZAA�\AE�A �A��A��A-A��AdZAK�A"�A��A5?A�^A`BA/AĜAz�A=qAM�A5?A{A�A
��A
1'A
bA	��A
  A	�A	A	hsA��AM�A�#A=qA1A��A�A?}A�A-AdZA+A��A��AffAv�A=qA�A�A�A�AA n�A {@��;@�$�@�Ĝ@�t�@�/@��@��`@��@�7L@�/@��/@��@�5?@���@� �@�C�@�ȴ@�z�@�t�@�\)@�ȴ@�G�@�9X@��;@�\)@��@�$�@�w@�@�\@�E�@�7@�?}@�%@�Ĝ@�D@�1@�+@�7L@�j@�z�@�9X@ߕ�@���@�J@�p�@�7L@��/@�Z@��;@ۍP@�\)@�@١�@�Ĝ@�Z@�1@���@�|�@�"�@�$�@թ�@ԣ�@�Q�@�1@�\)@�o@��@�ff@�?}@��`@д9@�z�@�1@��
@���@�dZ@�
=@���@�n�@�Ĝ@��@˅@��y@�5?@ə�@�p�@ȴ9@ǝ�@��@�V@���@�p�@��@�j@�1@öF@�;d@�"�@��y@�~�@�J@���@��^@��7@�hs@���@�9X@��@�ƨ@��w@��F@�t�@�+@��H@�E�@�5?@��@�@��@��@��T@�X@��j@�I�@���@�dZ@�o@���@�ff@��7@��@�dZ@�@���@�ȴ@�-@�V@��D@�I�@��m@��P@�@��R@�^5@�=q@�@��7@�Ĝ@�j@��
@�|�@��@�
=@��@�ff@�-@�J@�@���@���@��h@�x�@�p�@�`B@�G�@�?}@�&�@�V@���@��@��m@�;d@��@���@���@���@��\@�=q@��j@�1'@��@�1@��@�dZ@�;d@�33@���@�5?@�@��7@�V@�r�@� �@�1@���@�o@�{@��-@��7@�G�@��u@�(�@���@�K�@�33@��y@�=q@���@�G�@�&�@���@���@��9@���@��u@�z�@�j@�bN@�Z@�1@�l�@�@�n�@�=q@�5?@�$�@���@��7@��@�1@�t�@�C�@�;d@�;d@��@��!@�$�@��T@���@��7@�X@�&�@�Ĝ@���@�A�@��;@�l�@�S�@�33@�ȴ@�V@�@�p�@�X@���@�j@�9X@��@�|�@�33@���@��!@��\@�M�@�@��@�X@�%@���@�bN@�b@�ƨ@���@��P@��@�S�@�
=@���@��@��@��!@���@���@�E�@��@�@��T@���@�O�@���@���@�bN@�I�@�A�@�9X@�(�@�1@��@��w@��@�\)@�33@��@���@�~�@�-@���@���@���@��j@�r�@�bN@�Z@��;@�dZ@�+@�@��y@��@�M�@��T@��-@�`B@��@�V@�%@���@��`@���@�Ĝ@��9@��9@���@��@�Z@�9X@��@�P@~ȴ@~v�@}�T@}�@}/@|�D@{�
@{��@{�@{t�@{dZ@z��@z^5@y�#@xĜ@xbN@xb@w�;@w+@v{@up�@uV@t�/@t��@t��@t�D@tz�@tj@tZ@s��@sdZ@r�!@rM�@q��@qhs@q%@p �@o\)@o
=@nV@m�h@m`B@mV@l9X@j�@jn�@j^5@j=q@i��@i��@ihs@h�`@hA�@g�;@g\)@g;d@g
=@fV@e�-@e/@d��@c��@cdZ@b�H@b-@a�7@a&�@a�@a%@`��@`Ĝ@`��@`r�@`bN@` �@_��@_|�@^�y@^��@^5?@]�T@]��@]@]��@]p�@]?}@\�@\�/@\Z@\�@[�m@[�F@[@Z=q@Y��@Y��@Y�7@YG�@Y�@X�`@X��@X�@Xb@W�P@WK�@V�y@V�+@VE�@V@U@U/@T�j@Tj@S��@S�
@S�F@R��@Qhs@Q�@PĜ@P�@PbN@PA�@O�@O�@O�P@N��@N@M�-@M?}@L�@LZ@L�@K�m@K��@KC�@Ko@J��@J�\@JM�@J�@J�@JJ@I��@I&�@H�u@H �@G��@Gl�@G�@F�R@F@E��@E�-@E�h@E/@D�/@D�D@D�D@Dj@DI�@D�@D1@Cƨ@CC�@B�H@B�\@B=q@A�#@A�^@A�^@A�7@Ax�@AX@A7L@@��@@bN@?�@?\)@?�@>�R@=p�@<�/@<�@<�D@<I�@:�@:��@:�!@:�!@:�\@:J@9�7@8��@8r�@8bN@8A�@8b@7��@7\)@6��@6E�@5�@5�T@5@5��@5p�@5O�@5/@5�@5�@4�@4I�@3�m@3��@3o@2~�@1��@1��@1��@1hs@1&�@0�@/��@/K�@/
=@.�y@.ȴ@.ff@-�-@-��@-p�@-O�@-O�@-?}@-/@,�j@,(�@+��@+ƨ@+dZ@*��@*-@*J@)�@)�^@)hs@)%@(�u@(�@(r�@(Q�@(  @'��@'�P@'\)@';d@'�@&�y@&�R@&v�@&E�@&5?@&5?@&$�@&$�@&{@&@%�T@%��@%@%��@%p�@%O�@%/@$��@$�@$I�@#��@#dZ@#"�@"��@"�!@"��@"^5@"-@"�@"J@!��@!��@!�@!��@!x�@!&�@ �`@ ��@ ��@ ��@ �`@ �`@ �`@ Ĝ@ ��@ �u@ �u@ �u@ �u@ �u@ r�@  �@   @�@�@�@�w@l�@
=@�@ȴ@�R@��@�+@ff@V@5?@@@��@?}@V@�@z�@j@I�@9X@��@�F@��@t�@S�@C�@"�@�H@~�@^5@�@�@�#@��@x�@7L@%@��@�`@Ĝ@Ĝ@Q�@K�@+@�@v�@v�@ff@E�@`B@V@��@��@z�@I�@�@ƨ@��@��@��@��@t�@S�@33@o@@�H@��@~�@��@�@��@G�@�@��@��@Ĝ@�9@�u@b@�@|�@\)@�@��@v�@ff@@�@?}@�@��@j@I�@(�@1@�m@ƨ@��@��@��@t�@C�@33@"�@o@o@@1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
"�B
"�B
"�B
"�B
"�B
#�B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
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
#�B
#�B
#�B
#�B
$�B
%�B
-B
5?B
T�B
n�B
�/B<jB0!BB�B��B�XB�/B+BL�BM�BF�B@�B�B8RB?}B1'B�B�B��B�sB�wBÖB��Bk�B8RBhB
�B
�-B
ɺB
�9B
��B
jB
,B
�B
+B
-B	��B	�ZB	�sB	�TB	ȴB	��B	�DB	� B	P�B	z�B	�%B	y�B	gmB	@�B	�B	bB	K�B	5?B	)�B	8RB	'�B��B	DB	uB	K�B	[#B	y�B	�oB	�hB	��B	�!B	�wB	�B	�B	�ZB	�HB	��B	�BB	�B
bB
�B
�B
oB
  B
uB
{B
�B
�B
%�B
-B
1'B
0!B
0!B
/B
49B
<jB
?}B
G�B
J�B
J�B
K�B
K�B
J�B
K�B
K�B
P�B
Q�B
VB
S�B
R�B
VB
XB
XB
S�B
T�B
Q�B
M�B
XB
S�B
J�B
H�B
J�B
@�B
G�B
G�B
A�B
@�B
>wB
<jB
<jB
6FB
<jB
?}B
E�B
F�B
F�B
L�B
L�B
I�B
E�B
E�B
H�B
>wB
=qB
D�B
@�B
G�B
H�B
F�B
F�B
@�B
6FB
>wB
F�B
A�B
G�B
D�B
>wB
A�B
>wB
A�B
B�B
@�B
E�B
C�B
F�B
I�B
H�B
B�B
=qB
D�B
J�B
N�B
O�B
P�B
L�B
G�B
D�B
B�B
@�B
M�B
L�B
G�B
K�B
G�B
C�B
?}B
;dB
C�B
B�B
@�B
>wB
B�B
>wB
7LB
33B
2-B
+B
'�B
)�B
+B
-B
%�B
�B
#�B
�B
,B
33B
5?B
8RB
7LB
33B
,B
&�B
&�B
+B
'�B
&�B
�B
�B
�B
�B
�B
�B
#�B
!�B
 �B
�B
bB
�B
�B
�B
�B
�B
�B
�B
�B
�B
uB
JB
�B
�B
�B
�B
{B
oB
�B
�B
�B
{B
{B
{B
uB
bB
	7B
	7B
VB
VB
VB
JB

=B
B

=B
+B
JB
JB

=B
DB
DB
	7B
B
PB
\B
\B
VB
bB
hB
VB
VB
\B

=B
B
JB
JB
DB
JB
VB
oB
VB
JB
bB
{B
�B
�B
�B
�B
�B
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
 �B
 �B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
hB
�B
�B
 �B
�B
�B
�B
�B
 �B
 �B
 �B
�B
!�B
!�B
"�B
!�B
�B
�B
!�B
 �B
"�B
#�B
%�B
$�B
#�B
%�B
%�B
%�B
'�B
'�B
'�B
'�B
'�B
'�B
'�B
'�B
&�B
%�B
#�B
!�B
#�B
"�B
#�B
&�B
%�B
%�B
#�B
 �B
�B
!�B
%�B
%�B
$�B
"�B
$�B
$�B
"�B
!�B
#�B
$�B
#�B
"�B
%�B
'�B
%�B
"�B
!�B
'�B
(�B
(�B
&�B
(�B
(�B
+B
-B
+B
(�B
+B
.B
0!B
0!B
0!B
1'B
1'B
1'B
1'B
2-B
2-B
1'B
/B
-B
.B
.B
1'B
2-B
2-B
0!B
/B
.B
+B
/B
33B
49B
49B
2-B
0!B
0!B
33B
49B
49B
49B
33B
2-B
33B
2-B
2-B
2-B
5?B
5?B
2-B
2-B
2-B
49B
5?B
33B
2-B
6FB
49B
7LB
7LB
7LB
9XB
9XB
8RB
8RB
9XB
6FB
8RB
8RB
:^B
;dB
;dB
=qB
>wB
>wB
=qB
=qB
?}B
?}B
?}B
?}B
@�B
?}B
>wB
?}B
@�B
?}B
>wB
>wB
>wB
?}B
A�B
B�B
C�B
C�B
B�B
B�B
B�B
A�B
A�B
B�B
B�B
A�B
A�B
B�B
A�B
A�B
A�B
@�B
C�B
D�B
F�B
E�B
B�B
C�B
F�B
G�B
G�B
E�B
B�B
B�B
F�B
F�B
G�B
J�B
J�B
J�B
J�B
J�B
J�B
K�B
K�B
J�B
I�B
J�B
J�B
J�B
I�B
I�B
K�B
K�B
K�B
L�B
K�B
K�B
N�B
N�B
N�B
M�B
K�B
M�B
L�B
K�B
M�B
N�B
N�B
L�B
L�B
N�B
O�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
O�B
O�B
O�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
S�B
S�B
S�B
VB
T�B
S�B
S�B
W
B
YB
YB
XB
XB
XB
W
B
W
B
YB
YB
ZB
YB
XB
XB
YB
YB
YB
ZB
ZB
ZB
[#B
]/B
_;B
_;B
^5B
^5B
^5B
^5B
^5B
^5B
]/B
^5B
]/B
^5B
^5B
^5B
`BB
`BB
_;B
_;B
_;B
_;B
`BB
_;B
`BB
_;B
_;B
^5B
^5B
aHB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
aHB
bNB
bNB
bNB
bNB
cTB
cTB
cTB
bNB
cTB
dZB
dZB
dZB
dZB
aHB
`BB
dZB
dZB
dZB
dZB
dZB
dZB
dZB
dZB
cTB
dZB
ffB
ffB
ffB
ffB
gmB
gmB
gmB
gmB
gmB
hsB
hsB
hsB
hsB
iyB
hsB
gmB
gmB
hsB
iyB
iyB
iyB
iyB
iyB
hsB
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
k�B
jB
k�B
l�B
l�B
m�B
m�B
n�B
m�B
n�B
m�B
m�B
l�B
k�B
k�B
m�B
m�B
l�B
k�B
n�B
p�B
o�B
o�B
l�B
q�B
q�B
q�B
q�B
o�B
p�B
p�B
r�B
s�B
s�B
s�B
r�B
r�B
q�B
s�B
t�B
t�B
t�B
u�B
t�B
u�B
u�B
u�B
u�B
t�B
s�B
s�B
t�B
t�B
s�B
t�B
v�B
v�B
v�B
u�B
t�B
s�B
v�B
w�B
w�B
w�B
v�B
u�B
x�B
x�B
x�B
y�B
x�B
x�B
w�B
v�B
x�B
x�B
w�B
w�B
y�B
z�B
z�B
z�B
z�B
z�B
z�B
|�B
|�B
{�B
{�B
|�B
|�B
|�B
|�B
|�B
|�B
}�B
}�B
}�B
~�B
~�B
~�B
~�B
~�B
~�B
}�B
~�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
|�B
~�B
� B
� B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�%B
�%B
�%B
�%B
�%B
�%B
�%B
�%B
�%B
�%B
�%B
�%B
�%B
�+B
�1B
�+B
�1B
�+B
�+B
�1B
�1B
�1B
�1B
�1B
�1B
�+B
�7B
�7B
�7B
�7B
�7B
�7B
�7B
�=B
�=B
�=B
�=B
�=B
�1B
�+B
�DB
�=B
�=B
�JB
�DB
�DB
�7B
�DB
�PB
�PB
�PB
�PB
�PB
�PB
�\B
�\B
�\B
�\B
�VB
�\B
�\B
�\B
�\B
�\B
�\B
�VB
�VB
�bB
�\B
�\B
�bB
�hB
�hB
�hB
�hB
�hB
�bB
�bB
�hB
�oB
�hB
�oB
�uB
�uB
�hB
�bB
�oB
�oB
�oB
�uB
�{B
�{B
�{B
�{B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B
"�B
"�B
"�B
"�B
"�B
#�B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
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
#�B
#�B
#�B
#�B
$�B
%�B
-B
5?B
T�B
n�B
�/B<jB0!BB�B��B�XB�/B+BL�BM�BF�B@�B�B8RB?}B1'B�B�B��B�sB�wBÖB��Bk�B8RBhB
�B
�-B
ɺB
�9B
��B
jB
,B
�B
+B
-B	��B	�ZB	�sB	�TB	ȴB	��B	�DB	� B	P�B	z�B	�%B	y�B	gmB	@�B	�B	bB	K�B	5?B	)�B	8RB	'�B��B	DB	uB	K�B	[#B	y�B	�oB	�hB	��B	�!B	�wB	�B	�B	�ZB	�HB	��B	�BB	�B
bB
�B
�B
oB
  B
uB
{B
�B
�B
%�B
-B
1'B
0!B
0!B
/B
49B
<jB
?}B
G�B
J�B
J�B
K�B
K�B
J�B
K�B
K�B
P�B
Q�B
VB
S�B
R�B
VB
XB
XB
S�B
T�B
Q�B
M�B
XB
S�B
J�B
H�B
J�B
@�B
G�B
G�B
A�B
@�B
>wB
<jB
<jB
6FB
<jB
?}B
E�B
F�B
F�B
L�B
L�B
I�B
E�B
E�B
H�B
>wB
=qB
D�B
@�B
G�B
H�B
F�B
F�B
@�B
6FB
>wB
F�B
A�B
G�B
D�B
>wB
A�B
>wB
A�B
B�B
@�B
E�B
C�B
F�B
I�B
H�B
B�B
=qB
D�B
J�B
N�B
O�B
P�B
L�B
G�B
D�B
B�B
@�B
M�B
L�B
G�B
K�B
G�B
C�B
?}B
;dB
C�B
B�B
@�B
>wB
B�B
>wB
7LB
33B
2-B
+B
'�B
)�B
+B
-B
%�B
�B
#�B
�B
,B
33B
5?B
8RB
7LB
33B
,B
&�B
&�B
+B
'�B
&�B
�B
�B
�B
�B
�B
�B
#�B
!�B
 �B
�B
bB
�B
�B
�B
�B
�B
�B
�B
�B
�B
uB
JB
�B
�B
�B
�B
{B
oB
�B
�B
�B
{B
{B
{B
uB
bB
	7B
	7B
VB
VB
VB
JB

=B
B

=B
+B
JB
JB

=B
DB
DB
	7B
B
PB
\B
\B
VB
bB
hB
VB
VB
\B

=B
B
JB
JB
DB
JB
VB
oB
VB
JB
bB
{B
�B
�B
�B
�B
�B
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
 �B
 �B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
hB
�B
�B
 �B
�B
�B
�B
�B
 �B
 �B
 �B
�B
!�B
!�B
"�B
!�B
�B
�B
!�B
 �B
"�B
#�B
%�B
$�B
#�B
%�B
%�B
%�B
'�B
'�B
'�B
'�B
'�B
'�B
'�B
'�B
&�B
%�B
#�B
!�B
#�B
"�B
#�B
&�B
%�B
%�B
#�B
 �B
�B
!�B
%�B
%�B
$�B
"�B
$�B
$�B
"�B
!�B
#�B
$�B
#�B
"�B
%�B
'�B
%�B
"�B
!�B
'�B
(�B
(�B
&�B
(�B
(�B
+B
-B
+B
(�B
+B
.B
0!B
0!B
0!B
1'B
1'B
1'B
1'B
2-B
2-B
1'B
/B
-B
.B
.B
1'B
2-B
2-B
0!B
/B
.B
+B
/B
33B
49B
49B
2-B
0!B
0!B
33B
49B
49B
49B
33B
2-B
33B
2-B
2-B
2-B
5?B
5?B
2-B
2-B
2-B
49B
5?B
33B
2-B
6FB
49B
7LB
7LB
7LB
9XB
9XB
8RB
8RB
9XB
6FB
8RB
8RB
:^B
;dB
;dB
=qB
>wB
>wB
=qB
=qB
?}B
?}B
?}B
?}B
@�B
?}B
>wB
?}B
@�B
?}B
>wB
>wB
>wB
?}B
A�B
B�B
C�B
C�B
B�B
B�B
B�B
A�B
A�B
B�B
B�B
A�B
A�B
B�B
A�B
A�B
A�B
@�B
C�B
D�B
F�B
E�B
B�B
C�B
F�B
G�B
G�B
E�B
B�B
B�B
F�B
F�B
G�B
J�B
J�B
J�B
J�B
J�B
J�B
K�B
K�B
J�B
I�B
J�B
J�B
J�B
I�B
I�B
K�B
K�B
K�B
L�B
K�B
K�B
N�B
N�B
N�B
M�B
K�B
M�B
L�B
K�B
M�B
N�B
N�B
L�B
L�B
N�B
O�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
O�B
O�B
O�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
S�B
S�B
S�B
VB
T�B
S�B
S�B
W
B
YB
YB
XB
XB
XB
W
B
W
B
YB
YB
ZB
YB
XB
XB
YB
YB
YB
ZB
ZB
ZB
[#B
]/B
_;B
_;B
^5B
^5B
^5B
^5B
^5B
^5B
]/B
^5B
]/B
^5B
^5B
^5B
`BB
`BB
_;B
_;B
_;B
_;B
`BB
_;B
`BB
_;B
_;B
^5B
^5B
aHB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
aHB
bNB
bNB
bNB
bNB
cTB
cTB
cTB
bNB
cTB
dZB
dZB
dZB
dZB
aHB
`BB
dZB
dZB
dZB
dZB
dZB
dZB
dZB
dZB
cTB
dZB
ffB
ffB
ffB
ffB
gmB
gmB
gmB
gmB
gmB
hsB
hsB
hsB
hsB
iyB
hsB
gmB
gmB
hsB
iyB
iyB
iyB
iyB
iyB
hsB
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
k�B
jB
k�B
l�B
l�B
m�B
m�B
n�B
m�B
n�B
m�B
m�B
l�B
k�B
k�B
m�B
m�B
l�B
k�B
n�B
p�B
o�B
o�B
l�B
q�B
q�B
q�B
q�B
o�B
p�B
p�B
r�B
s�B
s�B
s�B
r�B
r�B
q�B
s�B
t�B
t�B
t�B
u�B
t�B
u�B
u�B
u�B
u�B
t�B
s�B
s�B
t�B
t�B
s�B
t�B
v�B
v�B
v�B
u�B
t�B
s�B
v�B
w�B
w�B
w�B
v�B
u�B
x�B
x�B
x�B
y�B
x�B
x�B
w�B
v�B
x�B
x�B
w�B
w�B
y�B
z�B
z�B
z�B
z�B
z�B
z�B
|�B
|�B
{�B
{�B
|�B
|�B
|�B
|�B
|�B
|�B
}�B
}�B
}�B
~�B
~�B
~�B
~�B
~�B
~�B
}�B
~�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
|�B
~�B
� B
� B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�%B
�%B
�%B
�%B
�%B
�%B
�%B
�%B
�%B
�%B
�%B
�%B
�%B
�+B
�1B
�+B
�1B
�+B
�+B
�1B
�1B
�1B
�1B
�1B
�1B
�+B
�7B
�7B
�7B
�7B
�7B
�7B
�7B
�=B
�=B
�=B
�=B
�=B
�1B
�+B
�DB
�=B
�=B
�JB
�DB
�DB
�7B
�DB
�PB
�PB
�PB
�PB
�PB
�PB
�\B
�\B
�\B
�\B
�VB
�\B
�\B
�\B
�\B
�\B
�\B
�VB
�VB
�bB
�\B
�\B
�bB
�hB
�hB
�hB
�hB
�hB
�bB
�bB
�hB
�oB
�hB
�oB
�uB
�uB
�hB
�bB
�oB
�oB
�oB
�uB
�{B
�{B
�{B
�{B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.01 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted during real time processing based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       20230302100141                              AO  ARCAADJP                                                                    20230302100141    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20230302100141  QCP$                G�O�G�O�G�O�1F83E           AO  ARGQQCPL                                                                    20230302100141  QCF$                G�O�G�O�G�O�0               