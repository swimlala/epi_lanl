CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  Y   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2014-07-22T01:11:09Z creation      
references        (http://www.argodatamgt.org/Documentation   comment           user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      
_FillValue               conventions       Argo reference table 1          6�   FORMAT_VERSION                 	long_name         File format version    
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
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      
_FillValue               conventions       Argo reference table 23          7�   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    7�   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
_FillValue        A.�~       axis      T      
resolution        >�E�vQ�        7�   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    7�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
_FillValue        A.�~       
resolution        >�E�vQ�        7�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8    CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9    PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9$   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9(   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9,   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        d  90   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 \  F�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     d  I�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 \  WT   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     d  Z�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     d  h   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 \  ux   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     d  x�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 \  �8   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     d  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     d  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 \  �\   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     d  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 \  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     d  �x   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    
_FillValue               conventions       YYYYMMDDHHMISS        ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �x   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �|   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    π   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    τ   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ψ   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     �8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     �X         �XArgo profile    3.1 1.2 19500101000000  20140722011109  20181023142215  5904271 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               A   AO  4744_0188_003                   2C  D   NAVIS_A                         863 @֘��@ 1   @֘ٷ�� @*��1'�c�+I�1   GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                      A   A   A   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�33B�ffB���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C�C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D=��D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ D�|�D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ Dɼ�D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�P D�` 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��R@�Q�A (�A (�A@(�A`(�A�{A�{A�{A�{A�{A�{A�{A�{B 
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
=B�B�B�8RB�k�B���B���B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�C �C�C�C�C�C
�C�C�C�C�C�C)C�C�C�C�C �C"�C$�C&�C(�C*�C,�C.�C0�C2�C4�C6�C8�C:�C<�C>�C@�CB�CD�CF�CH�CJ�CL�CN�CP�CR�CT�CV�CX�CZ�C\�C^�C`�Cb�Cd�Cf�Ch�Cj�Cl�Cn�Cp�Cr�Ct�Cv�Cx�Cz�C|�C~�C�HC�HC�HC�C�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�C�HC�HC�HC�HC�HC�HC�HC�HC�C�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HD  �D ��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D	 �D	��D
 �D
��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D  �D ��D! �D!��D" �D"��D# �D#��D$ �D$��D% �D%��D& �D&��D' �D'��D( �D(��D) �D)��D* �D*��D+ �D+��D, �D,��D- �D-��D. �D.��D/ �D/��D0 �D0��D1 �D1��D2 �D2��D3 �D3��D4 �D4��D5 �D5��D6 �D6��D7 �D7��D8 �D8��D9 �D9��D: �D:��D; �D;��D< �D<��D= �D=��D=�>D>��D? �D?��D@ �D@��DA �DA��DB �DB��DC �DC��DD �DD��DE �DE��DF �DF��DG �DG��DH �DH��DI �DI��DJ �DJ��DK �DK��DL �DL��DM �DM��DN �DN��DO �DO��DP �DP��DQ �DQ��DR �DR��DS �DS��DT �DT��DU �DU��DV �DV��DW �DW��DX �DX��DY �DY��DZ �DZ��D[ �D[��D\ �D\��D] �D]��D^ �D^��D_ �D_��D` �D`��Da �Da��Db �Db��Dc �Dc��Dd �Dd��De �De��Df �Df��Dg �Dg��Dh �Dh��Di �Di��Dj �Dj��Dk �Dk��Dl �Dl��Dm �Dm��Dn �Dn��Do �Do��Dp �Dp��Dq �Dq��Dr �Dr��Ds �Ds��Dt �Dt��Du �Du��Dv �Dv��Dw �Dw��Dx �Dx��Dy �Dy��Dz �Dz��D{ �D{��D| �D|��D} �D}��D~ �D~��D �D��D� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RDRD��RD� RD�@RD�}D��RD� RD�@RDĀRD��RD� RD�@RDŀRD��RD� RD�@RDƀRD��RD� RD�@RDǀRD��RD� RD�@RDȀRD��RD� RD�@RDɀRDɽD� RD�@RDʀRD��RD� RD�@RDˀRD��RD� RD�@RD̀RD��RD� RD�@RD̀RD��RD� RD�@RD΀RD��RD� RD�@RDπRD��RD� RD�@RDЀRD��RD� RD�@RDрRD��RD� RD�@RDҀRD��RD� RD�@RDӀRD��RD� RD�@RDԀRD��RD� RD�@RDՀRD��RD� RD�PRD�`R11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A���A��A��AԸRA԰!A�9XAӲ-A�C�AҴ9A҇+A�r�A�dZA�XA�M�A�G�A�A�A�7LA�/A�+A��A�{A�JA�A���A��yA��TA���Aџ�A�(�AиRAω7A�
=A��#A�A�dZAƙ�A�dZA���A���A�oA¥�A�XA�ƨA�&�A��PA�A�A��
A��7A�=qA�I�A�
=A��FA���A��mA���A���A���A�$�A�A���A���A���A�|�A���A��DA�S�A��A�jA�r�A�M�A��A�A�A��;A��7A���A�I�A��\A��!A��A�?}A�l�A��+A�A�1A�G�A���A�ZA���A�A��A}Ay��Av5?Aq+Ao7LAlA�Ai&�Ad��AaG�A]�PAXr�AS7LAQ�AMC�AJ��AG|�AF�!AE�-AB��A?�A>I�A=�;A=+A9�#A5��A3�wA1�A/�A/VA-`BA)�A*�HA)�#A(��A&��A%G�A$=qA"��A�wA��A�/A�!Av�A��A\)A�HA�uA�RA��A/A-A�A��AdZAƨA�7AoA�A��AO�A�A�;AA��AZA/A��A^5A�!AA�A��AƨAXA�7A��AƨA"�An�A�7A�9A1'AA�PA�A`BAZA�^A��AJA��A&�A�9A��AhsA
��A
ĜA
��A
M�A
A	�mA	�-A	��A
1'A
(�A	�A
1'A
ZA
=qA
$�A	\)AA�AbA��AXAQ�A�wAdZAG�AG�A7LA�yA��AoA��A-A�^Al�A`BAx�AƨA�FA�yA��A�+AjAffAA�A �A�
Al�A �@���@��!@�M�@��#@�`B@�bN@�|�@��y@���@�ff@�hs@��j@�bN@��F@�+@���@���@��9@�Q�@�ƨ@�S�@���@�@�%@�z�@�b@�dZ@���@�$�@��@�j@� �@�1@�t�@�~�@��@���@��@���@��;@�C�@�"�@��@�ff@�^@�X@���@�A�@�b@��@�K�@��@�ȴ@��T@�?}@�@�(�@߾w@�|�@�ȴ@�=q@�@�V@܃@۶F@��@�~�@�E�@�@�&�@� �@��@�=q@թ�@�X@��@ԓu@��m@ӍP@�C�@��@�@��@�j@�;d@�V@�5?@�@�X@��`@�z�@��
@�S�@�^5@���@�V@ȣ�@�(�@�l�@�;d@�33@ƸR@�$�@�@�7L@�V@��`@�r�@�(�@��;@�\)@��@�@��@�`B@���@�b@��P@�C�@�
=@��H@��\@�V@�@�%@���@��9@��@���@�bN@�\)@�
=@��H@��R@�5?@�&�@��9@�1'@��m@��@�;d@���@�=q@��#@��h@�O�@�V@��9@�j@�I�@�(�@� �@��@��@���@�K�@�@��+@�5?@��@��7@���@��j@���@�Q�@��@�33@���@�V@��@��^@�X@��@��9@�r�@�I�@�(�@�t�@�S�@�;d@�+@�@��@���@���@�n�@�5?@���@���@�%@��u@��;@�K�@�;d@�33@�+@�@���@���@�=q@��T@��^@��-@���@���@�hs@���@��j@���@�  @��
@�ƨ@��F@��@�o@�=q@���@���@�p�@���@��@�I�@�(�@�1@���@�dZ@�o@�v�@���@�G�@���@���@�Z@�I�@�(�@��;@��F@��H@��@��T@���@�X@�G�@�7L@�/@���@�Q�@�(�@��@���@�dZ@�33@�n�@�x�@�G�@��@��@��9@�z�@�1'@���@��P@�o@��@��y@�n�@��-@�p�@�`B@�/@���@���@��@��
@���@�dZ@�K�@�"�@���@�5?@�@��@�&�@���@���@�I�@�  @���@�
=@���@�~�@�^5@�E�@��@��@���@��9@��@��@��P@�"�@�o@���@�V@��^@���@��@�O�@�7L@��@��@���@��@�Z@��@�;d@�o@��@�v�@���@�?}@�&�@��@�V@��@���@�A�@��@\)@+@�@~5?@}�h@}?}@}/@}V@|�@|I�@|1@{ƨ@{t�@z�H@z��@z�!@z~�@z=q@y��@yX@y&�@x�`@x��@xr�@x �@w�@w|�@w
=@v�R@v��@v�+@vV@v{@u�T@u��@u�@u/@t��@t�@t(�@sƨ@st�@sC�@r�@r-@q�#@qX@pĜ@pA�@o�@o�P@n��@n�+@nV@n$�@m�T@m`B@l��@k�m@j��@jn�@jM�@jJ@i��@ihs@i%@hA�@g�@g�w@g�@g�P@f��@fff@f{@f@f$�@e�-@e�@dj@cdZ@b�!@b-@ax�@a�@`Ĝ@`��@`�u@`A�@_�;@_�w@_|�@_;d@^��@^{@]@]p�@\�@\9X@[��@[o@Z�!@Z^5@Y�@Y�^@Yx�@Y%@Xr�@W��@W\)@W
=@V�y@V�R@V�+@V$�@U�-@T��@T��@Tz�@T(�@S�
@S��@R�@RJ@Q�7@QX@Q7L@Q%@P��@PĜ@PQ�@Pb@O�;@O�@Ol�@O+@O+@O
=@N�R@Nv�@M�T@M��@M�h@M/@L��@Lz�@L(�@L�@L1@K��@K�
@KdZ@KdZ@K33@J�H@J��@J�\@J^5@J�@Ix�@H�9@HbN@HbN@HQ�@HA�@H  @G�;@G�w@G�P@G;d@F��@Fff@FV@F@E�T@E@E�@D�/@D�D@D(�@C�m@C��@C33@B��@B~�@B=q@B�@A�@Ahs@@�`@@��@@��@@Ĝ@@��@@r�@@  @?;d@>�y@>�+@>V@>5?@>@=��@=O�@<��@<j@<�@;t�@;@:��@9�@9&�@8�9@8Q�@8  @8  @8b@8b@8 �@7��@7+@6�R@6$�@5�@5@5��@5�@5p�@5?}@5�@5V@4�/@4�D@4I�@4�@3��@3dZ@3o@2�\@2-@1�^@1�7@1hs@17L@1%@0Ĝ@0��@0A�@0  @/\)@.�@.�+@.v�@.v�@.$�@-@-p�@-?}@-V@,�/@,��@,9X@,(�@,�@+��@+dZ@*��@*-@)��@)��@)7L@)&�@)%@(�u@( �@'��@'\)@&��@&��@&V@&$�@&@%�T@%�h@%?}@%/@$��@$�@$I�@#�@#t�@#33@"��@"M�@"=q@"-@!�@!��@!��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A���A��A��AԸRA԰!A�9XAӲ-A�C�AҴ9A҇+A�r�A�dZA�XA�M�A�G�A�A�A�7LA�/A�+A��A�{A�JA�A���A��yA��TA���Aџ�A�(�AиRAω7A�
=A��#A�A�dZAƙ�A�dZA���A���A�oA¥�A�XA�ƨA�&�A��PA�A�A��
A��7A�=qA�I�A�
=A��FA���A��mA���A���A���A�$�A�A���A���A���A�|�A���A��DA�S�A��A�jA�r�A�M�A��A�A�A��;A��7A���A�I�A��\A��!A��A�?}A�l�A��+A�A�1A�G�A���A�ZA���A�A��A}Ay��Av5?Aq+Ao7LAlA�Ai&�Ad��AaG�A]�PAXr�AS7LAQ�AMC�AJ��AG|�AF�!AE�-AB��A?�A>I�A=�;A=+A9�#A5��A3�wA1�A/�A/VA-`BA)�A*�HA)�#A(��A&��A%G�A$=qA"��A�wA��A�/A�!Av�A��A\)A�HA�uA�RA��A/A-A�A��AdZAƨA�7AoA�A��AO�A�A�;AA��AZA/A��A^5A�!AA�A��AƨAXA�7A��AƨA"�An�A�7A�9A1'AA�PA�A`BAZA�^A��AJA��A&�A�9A��AhsA
��A
ĜA
��A
M�A
A	�mA	�-A	��A
1'A
(�A	�A
1'A
ZA
=qA
$�A	\)AA�AbA��AXAQ�A�wAdZAG�AG�A7LA�yA��AoA��A-A�^Al�A`BAx�AƨA�FA�yA��A�+AjAffAA�A �A�
Al�A �@���@��!@�M�@��#@�`B@�bN@�|�@��y@���@�ff@�hs@��j@�bN@��F@�+@���@���@��9@�Q�@�ƨ@�S�@���@�@�%@�z�@�b@�dZ@���@�$�@��@�j@� �@�1@�t�@�~�@��@���@��@���@��;@�C�@�"�@��@�ff@�^@�X@���@�A�@�b@��@�K�@��@�ȴ@��T@�?}@�@�(�@߾w@�|�@�ȴ@�=q@�@�V@܃@۶F@��@�~�@�E�@�@�&�@� �@��@�=q@թ�@�X@��@ԓu@��m@ӍP@�C�@��@�@��@�j@�;d@�V@�5?@�@�X@��`@�z�@��
@�S�@�^5@���@�V@ȣ�@�(�@�l�@�;d@�33@ƸR@�$�@�@�7L@�V@��`@�r�@�(�@��;@�\)@��@�@��@�`B@���@�b@��P@�C�@�
=@��H@��\@�V@�@�%@���@��9@��@���@�bN@�\)@�
=@��H@��R@�5?@�&�@��9@�1'@��m@��@�;d@���@�=q@��#@��h@�O�@�V@��9@�j@�I�@�(�@� �@��@��@���@�K�@�@��+@�5?@��@��7@���@��j@���@�Q�@��@�33@���@�V@��@��^@�X@��@��9@�r�@�I�@�(�@�t�@�S�@�;d@�+@�@��@���@���@�n�@�5?@���@���@�%@��u@��;@�K�@�;d@�33@�+@�@���@���@�=q@��T@��^@��-@���@���@�hs@���@��j@���@�  @��
@�ƨ@��F@��@�o@�=q@���@���@�p�@���@��@�I�@�(�@�1@���@�dZ@�o@�v�@���@�G�@���@���@�Z@�I�@�(�@��;@��F@��H@��@��T@���@�X@�G�@�7L@�/@���@�Q�@�(�@��@���@�dZ@�33@�n�@�x�@�G�@��@��@��9@�z�@�1'@���@��P@�o@��@��y@�n�@��-@�p�@�`B@�/@���@���@��@��
@���@�dZ@�K�@�"�@���@�5?@�@��@�&�@���@���@�I�@�  @���@�
=@���@�~�@�^5@�E�@��@��@���@��9@��@��@��P@�"�@�o@���@�V@��^@���@��@�O�@�7L@��@��@���@��@�Z@��@�;d@�o@��@�v�@���@�?}@�&�@��@�V@��@���@�A�@��@\)@+@�@~5?@}�h@}?}@}/@}V@|�@|I�@|1@{ƨ@{t�@z�H@z��@z�!@z~�@z=q@y��@yX@y&�@x�`@x��@xr�@x �@w�@w|�@w
=@v�R@v��@v�+@vV@v{@u�T@u��@u�@u/@t��@t�@t(�@sƨ@st�@sC�@r�@r-@q�#@qX@pĜ@pA�@o�@o�P@n��@n�+@nV@n$�@m�T@m`B@l��@k�m@j��@jn�@jM�@jJ@i��@ihs@i%@hA�@g�@g�w@g�@g�P@f��@fff@f{@f@f$�@e�-@e�@dj@cdZ@b�!@b-@ax�@a�@`Ĝ@`��@`�u@`A�@_�;@_�w@_|�@_;d@^��@^{@]@]p�@\�@\9X@[��@[o@Z�!@Z^5@Y�@Y�^@Yx�@Y%@Xr�@W��@W\)@W
=@V�y@V�R@V�+@V$�@U�-@T��@T��@Tz�@T(�@S�
@S��@R�@RJ@Q�7@QX@Q7L@Q%@P��@PĜ@PQ�@Pb@O�;@O�@Ol�@O+@O+@O
=@N�R@Nv�@M�T@M��@M�h@M/@L��@Lz�@L(�@L�@L1@K��@K�
@KdZ@KdZ@K33@J�H@J��@J�\@J^5@J�@Ix�@H�9@HbN@HbN@HQ�@HA�@H  @G�;@G�w@G�P@G;d@F��@Fff@FV@F@E�T@E@E�@D�/@D�D@D(�@C�m@C��@C33@B��@B~�@B=q@B�@A�@Ahs@@�`@@��@@��@@Ĝ@@��@@r�@@  @?;d@>�y@>�+@>V@>5?@>@=��@=O�@<��@<j@<�@;t�@;@:��@9�@9&�@8�9@8Q�@8  @8  @8b@8b@8 �@7��@7+@6�R@6$�@5�@5@5��@5�@5p�@5?}@5�@5V@4�/@4�D@4I�@4�@3��@3dZ@3o@2�\@2-@1�^@1�7@1hs@17L@1%@0Ĝ@0��@0A�@0  @/\)@.�@.�+@.v�@.v�@.$�@-@-p�@-?}@-V@,�/@,��@,9X@,(�@,�@+��@+dZ@*��@*-@)��@)��@)7L@)&�@)%@(�u@( �@'��@'\)@&��@&��@&V@&$�@&@%�T@%�h@%?}@%/@$��@$�@$I�@#�@#t�@#33@"��@"M�@"=q@"-@!�@!��@!��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
�B
�B
�B
�B
�yB
�BB
�B
��B
�}B
�jB
�jB
�jB
�jB
�jB
�jB
�jB
�jB
�jB
�jB
�jB
�jB
�jB
�jB
�dB
�dB
�dB
�dB
�dB
�}B
��B
ȴB
��B
�BBbB�%BD�Bu�B�B��B��B��B��B��B�!B�XB�^B�jB�wB��B��B�qB�jB�jB��By�BJ�B8RBO�Bn�Bx�Bt�Bq�Bk�BbNBZBN�BL�BbNBW
BVBR�BH�B<jB7LB(�B��B�Bq�BXBC�B$�B
��B
��B
��B
�B
hsB
[#B
I�B
!�B
%B	�`B	��B	�B	�uB	�B	p�B	[#B	A�B	+B	�B��B�fB�)B�B��B��B�#B�B�B�B�B�B�B��B��B�B�B�yB�fB�HB�yB	hB	 �B	#�B	 �B	�B	�B	�B	hB	1B	%B��B	
=B	PB	DB		7B	
=B	oB	0!B	M�B	e`B	dZB	_;B	�B	�VB	�VB	�JB	�=B	�%B	v�B	o�B	iyB	bNB	k�B	w�B	�+B	��B	�jB	ɺB	ȴB	��B	�B	�B	�`B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	�B	�yB	�B	�sB	�fB	�ZB	�`B	�`B	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	��B	��B
B
PB
�B
�B
�B
�B
oB
+B
+B
%B
+B
B
B
B
B
%B
1B
1B
DB
bB
hB
bB
hB
uB
�B
!�B
(�B
,B
'�B
'�B
,B
+B
+B
)�B
)�B
)�B
,B
,B
+B
+B
)�B
(�B
'�B
&�B
%�B
$�B
%�B
$�B
#�B
"�B
"�B
!�B
!�B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
{B
uB
uB
uB
uB
oB
hB
bB
bB
bB
bB
\B
\B
\B
VB
VB
VB
VB
PB
PB
PB
JB
JB
JB
DB

=B

=B

=B
	7B
	7B
1B
1B
1B
1B
1B
1B
1B
+B
+B
+B
+B
+B
1B
1B
1B
1B
1B
1B
1B
1B
1B
+B
1B
1B
1B
	7B
	7B
1B
1B
+B
1B
1B
+B
1B
1B
	7B

=B

=B

=B

=B

=B

=B

=B

=B

=B

=B

=B

=B

=B
DB
DB
DB
DB
DB
DB
DB
JB
JB
JB
JB
DB
DB
JB
JB
JB
JB
DB
JB
JB
JB
JB
PB
PB
PB
PB
PB
PB
PB
VB
VB
VB
VB
VB
VB
VB
VB
VB
VB
VB
\B
\B
\B
\B
bB
bB
bB
bB
bB
bB
hB
hB
hB
hB
hB
hB
oB
oB
oB
oB
oB
oB
oB
oB
oB
oB
oB
oB
oB
oB
oB
oB
uB
uB
{B
�B
�B
�B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
�B
�B
�B
�B
 �B
 �B
 �B
!�B
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
$�B
%�B
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
,B
,B
-B
.B
.B
.B
/B
/B
/B
/B
.B
.B
/B
0!B
0!B
1'B
1'B
2-B
2-B
33B
33B
49B
49B
49B
49B
49B
49B
49B
49B
5?B
6FB
6FB
6FB
6FB
5?B
6FB
6FB
6FB
6FB
6FB
7LB
6FB
6FB
6FB
7LB
8RB
7LB
8RB
9XB
:^B
:^B
;dB
;dB
;dB
;dB
<jB
<jB
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
>wB
>wB
>wB
?}B
?}B
?}B
?}B
?}B
?}B
?}B
?}B
?}B
?}B
?}B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
A�B
A�B
A�B
B�B
B�B
B�B
B�B
B�B
B�B
B�B
C�B
C�B
D�B
E�B
D�B
E�B
D�B
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
G�B
G�B
G�B
F�B
G�B
H�B
H�B
I�B
J�B
J�B
K�B
J�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
L�B
L�B
L�B
M�B
M�B
M�B
N�B
N�B
N�B
N�B
N�B
N�B
O�B
O�B
O�B
P�B
P�B
O�B
O�B
O�B
P�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
R�B
S�B
S�B
S�B
S�B
S�B
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
T�B
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
W
B
W
B
W
B
W
B
W
B
W
B
W
B
W
B
W
B
W
B
W
B
W
B
W
B
W
B
W
B
W
B
W
B
W
B
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
W
B
W
B
XB
XB
XB
XB
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
YB
YB
YB
ZB
ZB
ZB
[#B
[#B
[#B
ZB
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
_;B
`BB
aHB
aHB
aHB
aHB
aHB
aHB
`BB
`BB
`BB
`BB
aHB
aHB
aHB
aHB
aHB
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
dZB
dZB
dZB
dZB
dZB
dZB
dZB
dZB
e`B
e`B
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
hsB
hsB
hsB
hsB
hsB
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
l�B
l�B
l�B
l�B
l�B
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
o�B
o�B
o�B
o�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B
��B
�B
�B
�B
�B
�>B
׺B
�B
�2B
��B
��B
��B
��B
��B
��B
��B
��B
�xB
��B
��B
��B
��B
��B
��B
�uB
��B
�B
��B
��B
�RB
�[B
�^B
�JB�B�BG.BwgB�OB�vB�\B��B�B�fB�yB��B�B��B��B�:B�|B��B�LB�&B�B� BQ�B=�BP{Bs�B��ByBwmBo�Bg�Bc�BU.BS�Bf�BW�BV�BV�BN(B=�B9�B1�B
{B�BwsB_BJ�B,�BB
�7B
�B
�gB
k~B
_B
V�B
+yB
aB	�[B	�
B	��B	�hB	�@B	t�B	auB	G�B	1B	�B	�B��B��B��B۞B�B��B�B�]B��B��B��B�rB	BB��B�kB�B�B�B�gB��B	SB	# B	' B	$)B	�B	�B	"B	�B	
.B	6B��B	{B	�B	�B		�B		�B	nB	,hB	KFB	h|B	g,B	[UB	�B	�B	��B	�B	�zB	�tB	y1B	q�B	oLB	b�B	iRB	ufB	�RB	�B	��B	��B	�xB	֗B	�B	٥B	�B	�EB	�(B	�`B	��B	��B	�B	��B	�iB	�9B	�2B	�QB	�=B	�SB	�uB	�B	��B	��B	�B	�=B	��B	�B	�"B	�B	�{B	�B	�8B	�B	�3B	�B
�B
eB
B
B
#B
5B
�B
�B
zB
VB

eB
�B
B
WB
B
_B
	B
�B
B
<B
8B
�B
CB
�B
aB
 �B
)5B
.1B
(FB
(�B
,TB
+B
+\B
*DB
*�B
*�B
-B
.sB
,B
+pB
*�B
)�B
)B
'�B
&�B
%B
&VB
&B
$�B
#LB
#�B
"�B
"�B
"B
 �B
>B
{B
TB
�B
�B
B
sB
<B
�B
~B
�B
B
�B
�B
�B
kB
�B
B
�B
B
�B
B
^B
�B
�B
PB
vB
�B
�B
qB
�B
�B
�B
�B
�B
�B
JB
2B
B
�B
�B
\B
B
B
HB
B
_B
QB

�B
	�B
	�B
	bB
	�B
	�B
	iB
�B
�B
�B
�B
B
�B
~B
B
	mB
	B
�B
	vB
	.B
WB
jB
�B
�B
�B
�B
�B
	VB
	�B

B
�B
�B
B
oB
=B
�B
�B
�B
	�B

oB

xB

�B

�B

�B

�B

�B

�B

�B
@B
B
<B

�B
�B
�B
~B
�B
�B
B
MB
�B
iB
SB
cB
�B
�B
�B
�B
�B
B
�B
�B
B
�B
�B
�B
B
B
�B
�B
�B
�B
�B
�B
�B
�B
^B
dB
�B
�B
�B
�B
B
�B
�B
�B
(B
�B
�B
�B
�B
BB
B
�B
�B
�B
�B
�B
�B
�B
�B
�B
EB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
0B
B
aB
CB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
PB
�B
�B
`B
�B
�B
�B
�B
nB
�B
�B
�B
$B
;B
:B
�B
�B
�B
�B
-B
B
�B
�B
-B
B
%B
B
�B
�B
B
�B
�B
�B
B
B
'B
�B
�B
�B
�B
�B
�B
�B
 !B
 JB
!	B
!�B
!�B
"B
#B
#B
#B
#B
#1B
#B
#]B
#eB
#�B
#�B
$wB
$�B
$,B
$�B
&"B
'aB
(;B
(�B
(@B
(IB
)6B
)B
)*B
)zB
)�B
)�B
*SB
*uB
+sB
+FB
+}B
,oB
,�B
-�B
.yB
.qB
.CB
/@B
/TB
/dB
/�B
/aB
.�B
/[B
0�B
0�B
1:B
1tB
2�B
2�B
3XB
3QB
4nB
4WB
4`B
4gB
4XB
4jB
4�B
5B
5�B
6zB
6tB
6�B
7OB
5�B
6eB
6RB
6YB
6sB
6�B
7�B
6�B
6�B
6eB
7]B
8�B
7�B
8�B
9eB
:qB
:�B
;�B
;�B
;�B
;�B
<�B
<�B
<uB
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
>�B
>�B
>�B
?�B
?�B
?�B
?�B
?�B
?�B
?�B
?�B
?�B
?�B
?�B
@�B
@�B
@�B
@�B
AB
@�B
@�B
@�B
A�B
A�B
A�B
B�B
B�B
B�B
B�B
B�B
B�B
B�B
D8B
DaB
D�B
E�B
D�B
E�B
D�B
D�B
EB
E�B
E�B
E�B
E�B
FB
FB
F�B
F�B
F�B
G�B
HB
H%B
GXB
H!B
IB
I(B
I�B
J�B
J�B
K�B
J�B
LB
K�B
K�B
K�B
L+B
L(B
K�B
K�B
M'B
MLB
M:B
N$B
NB
M�B
OB
N�B
N�B
OB
O)B
ORB
PB
PB
O�B
QB
QB
PB
P$B
PHB
QB
RB
RB
R!B
RB
RWB
R�B
SHB
TB
TB
TB
TB
TB
TGB
T#B
TB
TB
T&B
U*B
T�B
UB
U?B
U/B
UmB
V4B
VB
VSB
VOB
VCB
WKB
WB
WB
WB
W%B
WbB
W
B
W2B
WIB
W'B
W6B
W3B
WDB
W�B
W�B
WLB
WB
WB
WB
WBB
W&B
W)B
V/B
VOB
V�B
V<B
VB
VJB
W&B
W*B
WGB
W�B
WQB
W^B
XKB
X\B
XfB
X�B
X+B
XGB
X/B
XAB
X�B
Y�B
Y#B
YB
Y#B
Y2B
YCB
Y�B
Y�B
ZfB
ZoB
ZEB
[?B
[RB
[uB
ZeB
[nB
[�B
\xB
\�B
\�B
]�B
]�B
^�B
^�B
^�B
_~B
`@B
a:B
aHB
a?B
a�B
a�B
a�B
`�B
`kB
`jB
`[B
acB
aVB
anB
aaB
aWB
aqB
a�B
a�B
apB
b�B
b�B
b�B
b�B
c�B
c�B
czB
duB
d�B
d�B
d�B
dwB
d�B
d�B
d�B
e�B
e�B
fnB
fiB
f�B
f�B
f�B
g�B
g�B
g�B
g�B
g�B
h}B
h}B
h�B
h�B
h�B
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
l�B
l�B
l�B
l�B
l�B
m�B
m�B
m�B
m�B
nB
n�B
n�B
n�B
n�B
o�B
o�B
o�B
o�B
o�B
o�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<D�<r�|<#�
<#�
<#�
<#�
<0�><#�
<#�
<#�
<#�
<0��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<X�-<B�@<#�
<#�
<#�
<#�
<D��<+!�<b��<#�
<#�
<#�
<]�<</4�<c?<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<'�*<3��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<F�3<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - (REPORTED_SURFACE_PRESSURE) for PADJ                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            PSAL_ADJ corrects Cnd. Thermal Mass (CTM), Johnson et al,2007,JAOT & effects of pressure adjustments                                                                                                                                                            PADJ REPORTED_SURFACE_PRESSURE =-0.01 dbar                                                                                                                                                                                                                      none                                                                                                                                                                                                                                                            CTM alpha = 0.141 & tau = 6.68 s with error equal to the correction                                                                                                                                                                                             Pressures adjusted using reported surface pressure when warranted.                                                                                                                                                                                              none                                                                                                                                                                                                                                                            Salinity corrected for pressure sensor calibration drift and cell thermal mass                                                                                                                                                                                  201810180917422018101809174220181018091742  0188                            052512                          AO  ARCAADJP                                                                    20140722011109    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20140722011109  QCP$                G�O�G�O�G�O�DFB7E           AO  ARGQQCPL                                                                    20140722011109  QCF$                G�O�G�O�G�O�0                                                                                                                                   G�O�G�O�G�O�                PM  ARSQPADJV1.1                                                                20181018091742  QC  PRES            @�ffD�` G�O�                PM  ARSQCTM V1.1                                                                20181018091742  QC  PSAL            @�ffD�` G�O�                PM  ARSQOWGUV1.0                                                                20181023142215  IP                  G�O�G�O�G�O�                