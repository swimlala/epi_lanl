CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2022-03-17T10:02:29Z creation      
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
resolution        =���   axis      Z        X  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  H�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     X  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  [�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     X  _�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     X  o(   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ~�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     X  �X   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     X  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     X  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �8   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     X  �   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �h   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     X  �@   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ژ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
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
_FillValue                    �tArgo profile    3.1 1.2 19500101000000  20220317100229  20220317100229  5905729 Argo PMEL                                                       GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               �A   AO  7160                            2B  A   NAVIS_A                         0838                            170425                          863 @��V�B�1   @��W����@&bM����d?n��P1   GPS     Primary sampling: averaged                                                                                                                                                                                                                                         �A   A   A   @�ff@�  @���A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�33B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CG�fCJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� DfD� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D��3D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��R@�Q�@��A (�A@(�A`(�A�{A�{A�{A�{A�{A�{A�{A�{B 
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
=B�8RB���B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�C �C�C�C�C�C
�C�C�C�C�C�C�C�C�C�C�C �C"�C$�C&�C(�C*�C,�C.�C0�C2�C4�C6�C8�C:�C<�C>�C@�CB�CD�CF�CG��CJ�CL�CN�CP�CR�CT�CV�CX�CZ�C\�C^�C`�Cb�Cd�Cf�Ch�Cj�Cl�Cn�Cp�Cr�Ct�Cv�Cx�Cz�C|�C~�C�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�C�C�HC��{C�HC�HC�HC�HC�HC�HC�HC�HC�HC�HD  �D ��D �D��D �D��D �D��D �D��D
D��D �D��D �D��D �D��D	 �D	��D
 �D
��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D  �D ��D! �D!��D" �D"��D# �D#��D$ �D$��D% �D%��D& �D&��D' �D'��D( �D(��D) �D)��D* �D*��D+ �D+��D, �D,��D- �D-��D. �D.��D/ �D/��D0 �D0��D1 �D1��D2 �D2��D3 �D3��D4 �D4��D5 �D5��D6 �D6��D7 �D7��D8 �D8��D9 �D9��D: �D:��D; �D;��D< �D<��D= �D=��D> �D>��D? �D?��D@ �D@��DA �DA��DB �DB��DC �DC��DD �DD��DE �DE��DF �DF��DG �DG��DH �DH��DI �DI��DJ �DJ��DK �DK��DL �DL��DM �DM��DN �DN��DO �DO��DP �DP��DQ �DQ��DR �DR��DS �DS��DT �DT��DU �DU��DV �DV��DW �DW��DX �DX��DY �DY��DZ �DZ��D[ �D[��D\ �D\��D] �D]��D^ �D^��D_ �D_��D` �D`��Da �Da��Db �Db��Dc �Dc��Dd �Dd��De �De��Df �Df��Dg �Dg��Dh �Dh��Di �Di��Dj �Dj��Dk �Dk��Dl �Dl��Dm �Dm��Dn �Dn��Do �Do��Dp �Dp��Dq �Dq��Dr �Dr��Ds �Ds��Dt �Dt��Du �Du��Dv �Dv��Dw �Dw��Dx �Dx��Dy �Dy��Dz �Dz��D{ �D{��D| �D|��D} �D}��D~ �D~��D �D��D� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RDRD��RD� RD�@RDÀRD��RD� RD�@RDĀRD��RD� RD�@RDŀRD��RD� RD�@RDƀRD��RD� RD�@RDǀRD��RD� RD�@RDȀRD��RD� RD�@RDɀRD��RD� RD�@RDʀRD��RD� RD�@RDˀRD��RD� RD�@RD̀RD��RD� RD�@RD̀RD��RD� RD�@RD΀RD��RD� RD�@RDπRD��RD� RD�@RDЀRD��RD� RD�@RDрRD��RD� RD�@RDҀRD��RD� RD�@RDӀRD��RD� RD�@RDԀRD��RD� RD�@RDՀRD��RD� RD�@RDրRD��RD� RD�@RD׀RD��RD� RD�@RD؀RD��RD� RD�@RDـRD��RD� RD�@RDڀRD��RD� RD�@RDۀRD��RD� RD�@RD܀RD��RD� RD�@RD݀RD��RD� RD�@RDހRD��RD� RD�@RD߀RD��RD� RD�@RD��RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD��RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD���D��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�ffA�ffA�ffA�ffA�n�A�|�A�v�AօA֍PA֛�A֟�A֣�AָRA���A��A��TA���A�/A�l�A��#A�%A�JA�oA��A�7LA�;dA�1'A�(�A� �A�oA�Aѧ�A�-A���A�VA�ȴA�XA�I�A���A��A��;A���A�r�A�A��A�Q�A���A�S�A�t�A�7LA�l�A��FA�ffA���A���A�A��A�C�A�K�A�=qA�t�A�A{l�AvĜArĜAj�Ae�7A_�TA]oAY��ATv�APffAK��AI�AH-AGAEO�AD{AC;dAAS�A@�/A@VA@(�A?�TA?�7A?"�A>JA=�A=G�A;�FA:��A:A9l�A8��A81A7S�A6��A6jA5C�A4�yA4�HA4{A3p�A2�A2ffA1��A1+A0�uA0VA05?A01A/�mA/�7A.�A.M�A-�TA-|�A-"�A,ĜA,��A,r�A,=qA,JA+��A+�A+|�A+dZA+;dA*��A)�^A)x�A)?}A(�yA(n�A'�A'��A'��A'+A&ȴA&�9A&�A&��A&~�A&r�A&Q�A&-A%�FA%G�A$��A$�A$JA#hsA#/A"�yA"M�A!S�A ��A n�A Q�A E�A $�A�AĜA��AO�A+A
=A�A��AffA��A�-A�A��A5?A��A7LAv�A��A\)AO�A"�A�RA  A�wAx�A�A%A
=A%A�yA �AƨAx�A�A�HA�A��A~�A=qA{A��At�A�HA~�AE�A9XA5?A{A��A�AXA�AA��A�9A�A5?A��A?}AoA�RA �A
�`A
�uA
1A	�A�AQ�A{A�mA�;A�FAp�A��AjA1A|�A�A�AȴA�DAJAƨA��A`BAO�AK�A;dA�A��A�A�RAJA�wA�A��A�A/A �yA 1@�ƨ@���@�\)@�
=@��R@���@��+@��@�I�@��F@�t�@��@�5?@�x�@��@���@�Ĝ@�r�@��P@���@�$�@�@��^@��j@��T@�/@�Q�@@��y@�M�@��-@��/@�z�@��m@�S�@���@�v�@�-@��@�!@�ff@噚@�@���@�\@��T@�?}@���@��u@�r�@�1'@��@�C�@��@޸R@�^5@݉7@�r�@ۍP@��@�=q@��#@ى7@��@ץ�@�v�@��T@�G�@ԣ�@ӥ�@�
=@҇+@�J@ѡ�@�x�@�?}@�Ĝ@�I�@�|�@�;d@�o@�@�^5@���@�x�@�7L@���@���@�z�@�9X@���@ˍP@��@ʇ+@ɑh@�7L@���@� �@��@Ǖ�@��H@�J@��@ă@�I�@��@�K�@��H@��@���@�`B@�%@��9@�9X@��;@�ƨ@��y@�5?@��T@�hs@��`@��D@���@���@���@��@��@�(�@��;@�o@�ȴ@���@�V@���@�%@��/@���@�I�@���@�dZ@��@�5?@��7@�&�@��/@��9@��u@�I�@�  @���@�C�@��@��H@���@�M�@���@�(�@��H@���@�ff@�{@���@���@���@���@�hs@�7L@�V@��`@�z�@�I�@�9X@��;@�l�@��H@��@��h@��h@��@�hs@�G�@��@��@��`@��`@��/@��9@��u@�z�@�1'@�dZ@���@���@�^5@�$�@�@�x�@�?}@��u@�  @��@��F@�K�@���@��+@�@�@�x�@�V@�bN@�;d@�^5@�{@���@�G�@��/@�I�@���@���@���@���@�+@���@�v�@�=q@��-@�G�@�Ĝ@���@�r�@�I�@�b@��m@���@�o@�n�@��@��#@��@��#@���@�z�@��@��
@���@�t�@�K�@�+@���@�5?@��T@�x�@�7L@��@���@���@��`@��@�Z@��@��F@��P@�K�@�o@��H@�ȴ@���@��R@���@�ff@�$�@���@��h@�O�@�&�@���@���@�Z@��@��F@��P@��@�l�@�33@��y@�~�@�^5@�=q@�-@��@��@�O�@�V@��j@�z�@�bN@�9X@���@�K�@��@�5?@�{@��@�x�@�/@��@�%@��`@��j@�1'@l�@;d@+@~�@}`B@|�j@|(�@{�m@{�F@z~�@y��@y�^@y��@y7L@x��@xĜ@xĜ@x��@xbN@w�@w;d@vff@v5?@v$�@u�h@uV@t�j@t�D@tI�@t�@so@r�\@r�@q�^@qhs@q%@pbN@o�@o\)@nȴ@n$�@mp�@l��@l�j@l�D@l1@j��@j^5@ix�@h��@hr�@hA�@hb@g�@g��@gl�@f�R@fff@e�T@e�-@e�-@e�h@eO�@dI�@cS�@c@b�@b�H@b��@b~�@bM�@bM�@b=q@a&�@_�@^�@^��@]��@\��@\9X@\1@[�
@[�
@[�F@["�@Z��@Z^5@Y�@Y��@Y�@X��@XbN@W��@Wl�@W+@V�R@V��@V��@Vv�@V5?@V$�@U�T@U?}@T�@T�@T�/@T��@Tz�@T�@S��@R��@RJ@Q��@Q�^@Q��@Qhs@QX@Q&�@Q%@P��@P�@P1'@O��@O�@N�+@NV@N5?@N5?@N$�@N{@N{@N@M�@M�T@MO�@L�D@LI�@K�
@K�F@K��@Ko@J�!@JM�@JJ@Ix�@H�9@H1'@H  @G��@G�P@F��@F5?@F{@E`B@D�@D�j@D�@D��@Dj@C�m@C�F@C��@CC�@Co@B��@BM�@A�@A��@A�@@�@?�@?��@?|�@?+@>��@>�R@>��@>V@>@=��@=@=��@=p�@=/@<��@<I�@;ƨ@;��@;dZ@;o@:��@:��@:�!@:�@9�7@9hs@9&�@8�`@8Ĝ@8�9@8r�@8Q�@81'@7�w@7l�@7�@6��@6�@6��@6V@5��@5�h@5/@4�@4��@4j@4�@3�m@3�m@3�
@3�F@3�@3"�@2��@2�\@1��@1��@1�^@1��@1x�@1�7@1��@17L@1%@0Ĝ@0�u@0r�@0  @/\)@/+@.��@.ȴ@.�+@.5?@-��@-p�@-?}@,�/@,��@,��@,�j@,Z@+�m@+��@+dZ@+"�@*��@*�\@*^5@*J@)�^@)X@(�`@( �@'�@'�w@'�w@'�@'��@'�P@';d@&�+@&5?@&{@&@%��@%�h@%�@%?}@$�j@$9X@#�
@#��@#t�@#dZ@#S�@#S�@#33@"�H@"��@"~�@"M�@"=q@"�@!�#@!hs@!G�@!&�@ �`@ �u@ r�@ b@�@�;@�@\)@ff@$�@�@�T@��@��@�@p�@��@�@z�@(�@�F@S�@"�@o@��@��@�\@n�@=q@=q@�@J@��@�#@x�@&�@��@�`@��@��@�u@bN@1'@�@��@�w@��@�P@l�@\)@�@��@�@��@E�@@�-@`B@�@��@��@j@9X@��@�
@��@C�@�@�H@�H@��@�\@n�@^5@=q@-@�@��@�@�@��@��@x�@7L@��@Ĝ@��@�@bN@b@�w@��@|�@K�@+@
=@
=@�y@�R@�+@v�@V@5?@{@��@�h@p�@p�@`B@?}@�@�/@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�ffA�ffA�ffA�ffA�n�A�|�A�v�AօA֍PA֛�A֟�A֣�AָRA���A��A��TA���A�/A�l�A��#A�%A�JA�oA��A�7LA�;dA�1'A�(�A� �A�oA�Aѧ�A�-A���A�VA�ȴA�XA�I�A���A��A��;A���A�r�A�A��A�Q�A���A�S�A�t�A�7LA�l�A��FA�ffA���A���A�A��A�C�A�K�A�=qA�t�A�A{l�AvĜArĜAj�Ae�7A_�TA]oAY��ATv�APffAK��AI�AH-AGAEO�AD{AC;dAAS�A@�/A@VA@(�A?�TA?�7A?"�A>JA=�A=G�A;�FA:��A:A9l�A8��A81A7S�A6��A6jA5C�A4�yA4�HA4{A3p�A2�A2ffA1��A1+A0�uA0VA05?A01A/�mA/�7A.�A.M�A-�TA-|�A-"�A,ĜA,��A,r�A,=qA,JA+��A+�A+|�A+dZA+;dA*��A)�^A)x�A)?}A(�yA(n�A'�A'��A'��A'+A&ȴA&�9A&�A&��A&~�A&r�A&Q�A&-A%�FA%G�A$��A$�A$JA#hsA#/A"�yA"M�A!S�A ��A n�A Q�A E�A $�A�AĜA��AO�A+A
=A�A��AffA��A�-A�A��A5?A��A7LAv�A��A\)AO�A"�A�RA  A�wAx�A�A%A
=A%A�yA �AƨAx�A�A�HA�A��A~�A=qA{A��At�A�HA~�AE�A9XA5?A{A��A�AXA�AA��A�9A�A5?A��A?}AoA�RA �A
�`A
�uA
1A	�A�AQ�A{A�mA�;A�FAp�A��AjA1A|�A�A�AȴA�DAJAƨA��A`BAO�AK�A;dA�A��A�A�RAJA�wA�A��A�A/A �yA 1@�ƨ@���@�\)@�
=@��R@���@��+@��@�I�@��F@�t�@��@�5?@�x�@��@���@�Ĝ@�r�@��P@���@�$�@�@��^@��j@��T@�/@�Q�@@��y@�M�@��-@��/@�z�@��m@�S�@���@�v�@�-@��@�!@�ff@噚@�@���@�\@��T@�?}@���@��u@�r�@�1'@��@�C�@��@޸R@�^5@݉7@�r�@ۍP@��@�=q@��#@ى7@��@ץ�@�v�@��T@�G�@ԣ�@ӥ�@�
=@҇+@�J@ѡ�@�x�@�?}@�Ĝ@�I�@�|�@�;d@�o@�@�^5@���@�x�@�7L@���@���@�z�@�9X@���@ˍP@��@ʇ+@ɑh@�7L@���@� �@��@Ǖ�@��H@�J@��@ă@�I�@��@�K�@��H@��@���@�`B@�%@��9@�9X@��;@�ƨ@��y@�5?@��T@�hs@��`@��D@���@���@���@��@��@�(�@��;@�o@�ȴ@���@�V@���@�%@��/@���@�I�@���@�dZ@��@�5?@��7@�&�@��/@��9@��u@�I�@�  @���@�C�@��@��H@���@�M�@���@�(�@��H@���@�ff@�{@���@���@���@���@�hs@�7L@�V@��`@�z�@�I�@�9X@��;@�l�@��H@��@��h@��h@��@�hs@�G�@��@��@��`@��`@��/@��9@��u@�z�@�1'@�dZ@���@���@�^5@�$�@�@�x�@�?}@��u@�  @��@��F@�K�@���@��+@�@�@�x�@�V@�bN@�;d@�^5@�{@���@�G�@��/@�I�@���@���@���@���@�+@���@�v�@�=q@��-@�G�@�Ĝ@���@�r�@�I�@�b@��m@���@�o@�n�@��@��#@��@��#@���@�z�@��@��
@���@�t�@�K�@�+@���@�5?@��T@�x�@�7L@��@���@���@��`@��@�Z@��@��F@��P@�K�@�o@��H@�ȴ@���@��R@���@�ff@�$�@���@��h@�O�@�&�@���@���@�Z@��@��F@��P@��@�l�@�33@��y@�~�@�^5@�=q@�-@��@��@�O�@�V@��j@�z�@�bN@�9X@���@�K�@��@�5?@�{@��@�x�@�/@��@�%@��`@��j@�1'@l�@;d@+@~�@}`B@|�j@|(�@{�m@{�F@z~�@y��@y�^@y��@y7L@x��@xĜ@xĜ@x��@xbN@w�@w;d@vff@v5?@v$�@u�h@uV@t�j@t�D@tI�@t�@so@r�\@r�@q�^@qhs@q%@pbN@o�@o\)@nȴ@n$�@mp�@l��@l�j@l�D@l1@j��@j^5@ix�@h��@hr�@hA�@hb@g�@g��@gl�@f�R@fff@e�T@e�-@e�-@e�h@eO�@dI�@cS�@c@b�@b�H@b��@b~�@bM�@bM�@b=q@a&�@_�@^�@^��@]��@\��@\9X@\1@[�
@[�
@[�F@["�@Z��@Z^5@Y�@Y��@Y�@X��@XbN@W��@Wl�@W+@V�R@V��@V��@Vv�@V5?@V$�@U�T@U?}@T�@T�@T�/@T��@Tz�@T�@S��@R��@RJ@Q��@Q�^@Q��@Qhs@QX@Q&�@Q%@P��@P�@P1'@O��@O�@N�+@NV@N5?@N5?@N$�@N{@N{@N@M�@M�T@MO�@L�D@LI�@K�
@K�F@K��@Ko@J�!@JM�@JJ@Ix�@H�9@H1'@H  @G��@G�P@F��@F5?@F{@E`B@D�@D�j@D�@D��@Dj@C�m@C�F@C��@CC�@Co@B��@BM�@A�@A��@A�@@�@?�@?��@?|�@?+@>��@>�R@>��@>V@>@=��@=@=��@=p�@=/@<��@<I�@;ƨ@;��@;dZ@;o@:��@:��@:�!@:�@9�7@9hs@9&�@8�`@8Ĝ@8�9@8r�@8Q�@81'@7�w@7l�@7�@6��@6�@6��@6V@5��@5�h@5/@4�@4��@4j@4�@3�m@3�m@3�
@3�F@3�@3"�@2��@2�\@1��@1��@1�^@1��@1x�@1�7@1��@17L@1%@0Ĝ@0�u@0r�@0  @/\)@/+@.��@.ȴ@.�+@.5?@-��@-p�@-?}@,�/@,��@,��@,�j@,Z@+�m@+��@+dZ@+"�@*��@*�\@*^5@*J@)�^@)X@(�`@( �@'�@'�w@'�w@'�@'��@'�P@';d@&�+@&5?@&{@&@%��@%�h@%�@%?}@$�j@$9X@#�
@#��@#t�@#dZ@#S�@#S�@#33@"�H@"��@"~�@"M�@"=q@"�@!�#@!hs@!G�@!&�@ �`@ �u@ r�@ b@�@�;@�@\)@ff@$�@�@�T@��@��@�@p�@��@�@z�@(�@�F@S�@"�@o@��@��@�\@n�@=q@=q@�@J@��@�#@x�@&�@��@�`@��@��@�u@bN@1'@�@��@�w@��@�P@l�@\)@�@��@�@��@E�@@�-@`B@�@��@��@j@9X@��@�
@��@C�@�@�H@�H@��@�\@n�@^5@=q@-@�@��@�@�@��@��@x�@7L@��@Ĝ@��@�@bN@b@�w@��@|�@K�@+@
=@
=@�y@�R@�+@v�@V@5?@{@��@�h@p�@p�@`B@?}@�@�/@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�'B	�'B	�?B	�wB	ƨB	��B	�B	�TB	��B
�B
E�B
S�B
VB
ZB
aHB
ffB
gmB
ffB
e`B
cTB
_;B
T�B
+B
P�B
B2-BF�BXBQ�BK�B?}BC�B2-B"�B�B�BbB  B
�ZB
�B
��B
�+B
�7B
�%B
v�B
T�B
A�B
#�B
)�B
oB
  B	�)B	��B	�bB	�+B	2-B	M�B	A�B	Q�B	E�B	+B	;dB	\)B	��B	�LB	�wB	��B	��B
VB
oB
-B
:^B
K�B
K�B
M�B
M�B
M�B
`BB
q�B
jB
|�B
�DB
�VB
�\B
�{B
��B
��B
��B
��B
��B
�?B
�!B
�?B
�RB
�wB
�}B
�dB
��B
ǮB
ȴB
ǮB
ǮB
ĜB
��B
��B
ĜB
ŢB
ǮB
ɺB
��B
��B
��B
��B
��B
��B
��B
��B
ȴB
B
�wB
ɺB
ɺB
ƨB
ƨB
ĜB
ɺB
ȴB
ĜB
ÖB
ȴB
ɺB
ȴB
ǮB
ǮB
ŢB
ÖB
�wB
�jB
�jB
�dB
�dB
�3B
�FB
�-B
�B
��B
��B
�B
�B
�B
��B
��B
��B
�oB
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
�oB
�hB
�\B
�\B
�{B
��B
��B
�\B
�DB
�\B
�\B
�PB
�hB
�uB
�oB
�\B
�+B
�DB
�JB
�DB
�=B
�DB
�JB
�DB
�1B
�%B
�B
~�B
z�B
}�B
~�B
�B
�B
� B
|�B
~�B
~�B
{�B
|�B
z�B
y�B
v�B
r�B
n�B
l�B
n�B
jB
e`B
[#B
aHB
^5B
[#B
YB
YB
_;B
_;B
_;B
\)B
W
B
Q�B
Q�B
Q�B
N�B
P�B
R�B
S�B
P�B
L�B
N�B
S�B
Q�B
S�B
T�B
R�B
Q�B
P�B
O�B
L�B
H�B
L�B
Q�B
P�B
N�B
J�B
I�B
C�B
K�B
N�B
L�B
L�B
J�B
L�B
J�B
E�B
<jB
C�B
E�B
B�B
A�B
A�B
C�B
E�B
D�B
A�B
<jB
>wB
=qB
?}B
:^B
33B
'�B
5?B
49B
2-B
33B
33B
2-B
0!B
33B
2-B
1'B
1'B
/B
(�B
$�B
$�B
,B
(�B
$�B
(�B
"�B
)�B
+B
,B
-B
-B
,B
+B
(�B
+B
)�B
'�B
#�B
 �B
"�B
#�B
%�B
%�B
$�B
 �B
�B
�B
!�B
 �B
�B
�B
�B
 �B
!�B
!�B
"�B
 �B
�B
�B
�B
!�B
"�B
!�B
�B
�B
�B
!�B
!�B
!�B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
uB
uB
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
\B
oB
�B
�B
�B
 �B
"�B
"�B
!�B
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
"�B
&�B
&�B
&�B
&�B
&�B
&�B
(�B
(�B
'�B
&�B
&�B
%�B
#�B
!�B
"�B
'�B
'�B
&�B
&�B
&�B
&�B
"�B
#�B
)�B
(�B
&�B
'�B
&�B
&�B
+B
)�B
(�B
$�B
"�B
&�B
.B
-B
.B
.B
-B
0!B
1'B
49B
33B
0!B
0!B
1'B
2-B
/B
1'B
1'B
5?B
5?B
5?B
5?B
5?B
49B
1'B
0!B
2-B
5?B
6FB
49B
2-B
)�B
33B
5?B
6FB
7LB
6FB
6FB
49B
33B
6FB
7LB
9XB
;dB
<jB
<jB
;dB
9XB
9XB
8RB
<jB
=qB
<jB
=qB
>wB
@�B
@�B
@�B
>wB
>wB
=qB
=qB
?}B
?}B
A�B
?}B
?}B
>wB
>wB
A�B
A�B
B�B
A�B
@�B
?}B
?}B
B�B
C�B
D�B
B�B
A�B
B�B
C�B
B�B
D�B
E�B
C�B
A�B
A�B
C�B
A�B
G�B
G�B
E�B
G�B
J�B
J�B
I�B
H�B
D�B
F�B
J�B
J�B
H�B
C�B
I�B
J�B
K�B
K�B
H�B
L�B
O�B
O�B
O�B
O�B
Q�B
P�B
O�B
N�B
L�B
M�B
L�B
O�B
O�B
N�B
N�B
P�B
Q�B
Q�B
P�B
M�B
P�B
Q�B
Q�B
R�B
Q�B
P�B
R�B
Q�B
R�B
Q�B
R�B
T�B
VB
T�B
R�B
P�B
S�B
S�B
VB
W
B
YB
YB
YB
XB
VB
T�B
W
B
XB
YB
ZB
XB
W
B
S�B
W
B
[#B
^5B
^5B
^5B
]/B
]/B
]/B
ZB
T�B
T�B
VB
ZB
XB
YB
^5B
`BB
`BB
aHB
`BB
]/B
]/B
_;B
_;B
^5B
^5B
^5B
`BB
_;B
`BB
aHB
aHB
cTB
dZB
cTB
bNB
cTB
bNB
aHB
cTB
e`B
e`B
dZB
bNB
bNB
aHB
`BB
cTB
ffB
gmB
gmB
gmB
hsB
gmB
gmB
gmB
ffB
ffB
ffB
e`B
ffB
iyB
jB
jB
jB
jB
jB
jB
iyB
hsB
ffB
ffB
iyB
jB
k�B
k�B
hsB
iyB
jB
jB
hsB
gmB
jB
m�B
m�B
l�B
iyB
k�B
m�B
k�B
m�B
o�B
o�B
o�B
n�B
m�B
o�B
o�B
m�B
n�B
m�B
m�B
n�B
m�B
m�B
m�B
n�B
p�B
p�B
p�B
q�B
q�B
r�B
r�B
r�B
s�B
t�B
s�B
r�B
r�B
q�B
r�B
r�B
t�B
u�B
t�B
t�B
u�B
u�B
r�B
r�B
u�B
u�B
u�B
v�B
v�B
u�B
u�B
u�B
t�B
u�B
u�B
w�B
w�B
v�B
u�B
t�B
u�B
u�B
v�B
x�B
x�B
x�B
y�B
y�B
y�B
y�B
x�B
w�B
w�B
x�B
w�B
z�B
{�B
{�B
{�B
|�B
|�B
z�B
{�B
{�B
{�B
{�B
y�B
y�B
{�B
|�B
{�B
{�B
{�B
z�B
}�B
}�B
}�B
� B
� B
~�B
}�B
}�B
~�B
� B
� B
~�B
�B
�B
� B
� B
~�B
~�B
}�B
�B
�B
�B
�B
�B
�B
�B
� B
�B
�%B
�%B
�B
�B
�%B
�B
�B
�B
�%B
�+B
�1B
�1B
�1B
�1B
�1B
�+B
�1B
�1B
�1B
�7B
�1B
�+B
�+B
�7B
�7B
�1B
�1B
�7B
�7B
�=B
�=B
�7B
�1B
�%B
�DB
�JB
�PB
�PB
�JB
�JB
�JB
�DB
�=B
�DB
�=B
�7B
�DB
�DB
�JB
�JB
�JB
�PB
�PB
�PB
�PB
�PB
�PB
�PB
�JB
�JB
�JB
�VB
�\B
�VB
�VB
�\B
�VB
�VB
�\B
�bB
�bB
�bB
�bB
�bB
�bB
�\B
�bB
�bB
�\B
�\B
�\B
�\B
�bB
�bB
�hB
�bB
�hB
�hB
�hB
�hB
�bB
�bB
�hB
�oB
�oB
�oB
�hB
�oB
�oB
�oB
�uB
�uB
�oB
�uB
�uB
�uB
�oB
�oB
�oB
�oB
�oB
�uB
�uB
�uB
�oB
�uB
�{B
�{B
�{B
�{B
��B
��B
�{B
�{B
�{B
��B
��B
��B
��B
�{B
��B
��B
��B
��B
��B
��B
��B
��B
��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�'B	�'B	�?B	�wB	ƨB	��B	�B	�TB	��B
�B
E�B
S�B
VB
ZB
aHB
ffB
gmB
ffB
e`B
cTB
_;B
T�B
+B
P�B
B2-BF�BXBQ�BK�B?}BC�B2-B"�B�B�BbB  B
�ZB
�B
��B
�+B
�7B
�%B
v�B
T�B
A�B
#�B
)�B
oB
  B	�)B	��B	�bB	�+B	2-B	M�B	A�B	Q�B	E�B	+B	;dB	\)B	��B	�LB	�wB	��B	��B
VB
oB
-B
:^B
K�B
K�B
M�B
M�B
M�B
`BB
q�B
jB
|�B
�DB
�VB
�\B
�{B
��B
��B
��B
��B
��B
�?B
�!B
�?B
�RB
�wB
�}B
�dB
��B
ǮB
ȴB
ǮB
ǮB
ĜB
��B
��B
ĜB
ŢB
ǮB
ɺB
��B
��B
��B
��B
��B
��B
��B
��B
ȴB
B
�wB
ɺB
ɺB
ƨB
ƨB
ĜB
ɺB
ȴB
ĜB
ÖB
ȴB
ɺB
ȴB
ǮB
ǮB
ŢB
ÖB
�wB
�jB
�jB
�dB
�dB
�3B
�FB
�-B
�B
��B
��B
�B
�B
�B
��B
��B
��B
�oB
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
�oB
�hB
�\B
�\B
�{B
��B
��B
�\B
�DB
�\B
�\B
�PB
�hB
�uB
�oB
�\B
�+B
�DB
�JB
�DB
�=B
�DB
�JB
�DB
�1B
�%B
�B
~�B
z�B
}�B
~�B
�B
�B
� B
|�B
~�B
~�B
{�B
|�B
z�B
y�B
v�B
r�B
n�B
l�B
n�B
jB
e`B
[#B
aHB
^5B
[#B
YB
YB
_;B
_;B
_;B
\)B
W
B
Q�B
Q�B
Q�B
N�B
P�B
R�B
S�B
P�B
L�B
N�B
S�B
Q�B
S�B
T�B
R�B
Q�B
P�B
O�B
L�B
H�B
L�B
Q�B
P�B
N�B
J�B
I�B
C�B
K�B
N�B
L�B
L�B
J�B
L�B
J�B
E�B
<jB
C�B
E�B
B�B
A�B
A�B
C�B
E�B
D�B
A�B
<jB
>wB
=qB
?}B
:^B
33B
'�B
5?B
49B
2-B
33B
33B
2-B
0!B
33B
2-B
1'B
1'B
/B
(�B
$�B
$�B
,B
(�B
$�B
(�B
"�B
)�B
+B
,B
-B
-B
,B
+B
(�B
+B
)�B
'�B
#�B
 �B
"�B
#�B
%�B
%�B
$�B
 �B
�B
�B
!�B
 �B
�B
�B
�B
 �B
!�B
!�B
"�B
 �B
�B
�B
�B
!�B
"�B
!�B
�B
�B
�B
!�B
!�B
!�B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
uB
uB
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
\B
oB
�B
�B
�B
 �B
"�B
"�B
!�B
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
"�B
&�B
&�B
&�B
&�B
&�B
&�B
(�B
(�B
'�B
&�B
&�B
%�B
#�B
!�B
"�B
'�B
'�B
&�B
&�B
&�B
&�B
"�B
#�B
)�B
(�B
&�B
'�B
&�B
&�B
+B
)�B
(�B
$�B
"�B
&�B
.B
-B
.B
.B
-B
0!B
1'B
49B
33B
0!B
0!B
1'B
2-B
/B
1'B
1'B
5?B
5?B
5?B
5?B
5?B
49B
1'B
0!B
2-B
5?B
6FB
49B
2-B
)�B
33B
5?B
6FB
7LB
6FB
6FB
49B
33B
6FB
7LB
9XB
;dB
<jB
<jB
;dB
9XB
9XB
8RB
<jB
=qB
<jB
=qB
>wB
@�B
@�B
@�B
>wB
>wB
=qB
=qB
?}B
?}B
A�B
?}B
?}B
>wB
>wB
A�B
A�B
B�B
A�B
@�B
?}B
?}B
B�B
C�B
D�B
B�B
A�B
B�B
C�B
B�B
D�B
E�B
C�B
A�B
A�B
C�B
A�B
G�B
G�B
E�B
G�B
J�B
J�B
I�B
H�B
D�B
F�B
J�B
J�B
H�B
C�B
I�B
J�B
K�B
K�B
H�B
L�B
O�B
O�B
O�B
O�B
Q�B
P�B
O�B
N�B
L�B
M�B
L�B
O�B
O�B
N�B
N�B
P�B
Q�B
Q�B
P�B
M�B
P�B
Q�B
Q�B
R�B
Q�B
P�B
R�B
Q�B
R�B
Q�B
R�B
T�B
VB
T�B
R�B
P�B
S�B
S�B
VB
W
B
YB
YB
YB
XB
VB
T�B
W
B
XB
YB
ZB
XB
W
B
S�B
W
B
[#B
^5B
^5B
^5B
]/B
]/B
]/B
ZB
T�B
T�B
VB
ZB
XB
YB
^5B
`BB
`BB
aHB
`BB
]/B
]/B
_;B
_;B
^5B
^5B
^5B
`BB
_;B
`BB
aHB
aHB
cTB
dZB
cTB
bNB
cTB
bNB
aHB
cTB
e`B
e`B
dZB
bNB
bNB
aHB
`BB
cTB
ffB
gmB
gmB
gmB
hsB
gmB
gmB
gmB
ffB
ffB
ffB
e`B
ffB
iyB
jB
jB
jB
jB
jB
jB
iyB
hsB
ffB
ffB
iyB
jB
k�B
k�B
hsB
iyB
jB
jB
hsB
gmB
jB
m�B
m�B
l�B
iyB
k�B
m�B
k�B
m�B
o�B
o�B
o�B
n�B
m�B
o�B
o�B
m�B
n�B
m�B
m�B
n�B
m�B
m�B
m�B
n�B
p�B
p�B
p�B
q�B
q�B
r�B
r�B
r�B
s�B
t�B
s�B
r�B
r�B
q�B
r�B
r�B
t�B
u�B
t�B
t�B
u�B
u�B
r�B
r�B
u�B
u�B
u�B
v�B
v�B
u�B
u�B
u�B
t�B
u�B
u�B
w�B
w�B
v�B
u�B
t�B
u�B
u�B
v�B
x�B
x�B
x�B
y�B
y�B
y�B
y�B
x�B
w�B
w�B
x�B
w�B
z�B
{�B
{�B
{�B
|�B
|�B
z�B
{�B
{�B
{�B
{�B
y�B
y�B
{�B
|�B
{�B
{�B
{�B
z�B
}�B
}�B
}�B
� B
� B
~�B
}�B
}�B
~�B
� B
� B
~�B
�B
�B
� B
� B
~�B
~�B
}�B
�B
�B
�B
�B
�B
�B
�B
� B
�B
�%B
�%B
�B
�B
�%B
�B
�B
�B
�%B
�+B
�1B
�1B
�1B
�1B
�1B
�+B
�1B
�1B
�1B
�7B
�1B
�+B
�+B
�7B
�7B
�1B
�1B
�7B
�7B
�=B
�=B
�7B
�1B
�%B
�DB
�JB
�PB
�PB
�JB
�JB
�JB
�DB
�=B
�DB
�=B
�7B
�DB
�DB
�JB
�JB
�JB
�PB
�PB
�PB
�PB
�PB
�PB
�PB
�JB
�JB
�JB
�VB
�\B
�VB
�VB
�\B
�VB
�VB
�\B
�bB
�bB
�bB
�bB
�bB
�bB
�\B
�bB
�bB
�\B
�\B
�\B
�\B
�bB
�bB
�hB
�bB
�hB
�hB
�hB
�hB
�bB
�bB
�hB
�oB
�oB
�oB
�hB
�oB
�oB
�oB
�uB
�uB
�oB
�uB
�uB
�uB
�oB
�oB
�oB
�oB
�oB
�uB
�uB
�uB
�oB
�uB
�{B
�{B
�{B
�{B
��B
��B
�{B
�{B
�{B
��B
��B
��B
��B
�{B
��B
��B
��B
��B
��B
��B
��B
��B
��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.01 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20220317100229                              AO  ARCAADJP                                                                    20220317100229    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20220317100229  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20220317100229  QCF$                G�O�G�O�G�O�0               