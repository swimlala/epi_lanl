CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2023-02-10T08:01:32Z creation      
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
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  `x   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  p   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �4   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �T   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �<   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �\   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ݌   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �,   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �0   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �4   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �8Argo profile    3.1 1.2 19500101000000  20230210080132  20230210080132  5905729 Argo PMEL                                                       GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               �A   AO  7160                            2B  A   NAVIS_A                         0838                            170425                          863 @�ә8�(1   @��8�0@*XbM���d�A�7K�1   GPS     Primary sampling: averaged                                                                                                                                                                                                                                         �A   A   A   @�ff@�  A   A!��A@  A^ffA�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B ffB(  B0  B8  B?��BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�33B�33B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2�C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D'��D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� DlfDl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D���D�<�D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D�|�D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��R@�Q�A (�A!A@(�A^�\A�{A�{A�{A�{A�{A�{A�{A�{B 
=B
=B
=B
=B p�B(
=B0
=B8
=B?��BH
=BP
=BX
=B`
=Bh
=Bp
=Bx
=B�B�B�B�8RB�8RB���B���B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�C �C�C�C�C�C
�C�C�C�C�C�C�C�C�C�C�C �C"�C$�C&�C(�C*�C,�C.�C0�C2)C4�C6�C8�C:�C<�C>�C@�CB�CD�CF�CH�CJ�CL�CN�CP�CR�CT�CV�CX�CZ�C\�C^�C`�Cb�Cd�Cf�Ch�Cj�Cl�Cn�Cp�Cr�Ct�Cv�Cx�Cz�C|�C~�C�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�C�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC��{C�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HD  �D ��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D	 �D	��D
 �D
��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D  �D ��D! �D!��D" �D"��D# �D#��D$ �D$��D% �D%��D& �D&��D' �D'��D'�>D(��D) �D)��D* �D*��D+ �D+��D, �D,��D- �D-��D. �D.��D/ �D/��D0 �D0��D1 �D1��D2 �D2��D3 �D3��D4 �D4��D5 �D5��D6 �D6��D7 �D7��D8 �D8��D9 �D9��D: �D:��D; �D;��D< �D<��D= �D=��D> �D>��D? �D?��D@ �D@��DA �DA��DB �DB��DC �DC��DD �DD��DE �DE��DF �DF��DG �DG��DH �DH��DI �DI��DJ �DJ��DK �DK��DL �DL��DM �DM��DN �DN��DO �DO��DP �DP��DQ �DQ��DR �DR��DS �DS��DT �DT��DU �DU��DV �DV��DW �DW��DX �DX��DY �DY��DZ �DZ��D[ �D[��D\ �D\��D] �D]��D^ �D^��D_ �D_��D` �D`��Da �Da��Db �Db��Dc �Dc��Dd �Dd��De �De��Df �Df��Dg �Dg��Dh �Dh��Di �Di��Dj �Dj��Dk �Dk��Dl
Dl��Dm �Dm��Dn �Dn��Do �Do��Dp �Dp��Dq �Dq��Dr �Dr��Ds �Ds��Dt �Dt��Du �Du��Dv �Dv��Dw �Dw��Dx �Dx��Dy �Dy��Dz �Dz��D{ �D{��D| �D|��D} �D}��D~ �D~��D �D��D� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��D� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RDRD��RD� RD�@RDÀRD��RD� RD�@RDĀRD��RD� RD�@RDŀRD��RD� RD�@RDƀRD��RD� RD�@RDǀRD��RD� RD�@RDȀRD��RD� RD�@RDɀRD��RD� RD�@RDʀRD��RD� RD�@RDˀRD��RD� RD�@RD̀RD��RD� RD�@RD̀RD��RD� RD�@RD΀RD��RD� RD�@RDπRD��RD� RD�@RDЀRD��RD� RD�@RDрRD��RD� RD�@RDҀRD��RD� RD�@RDӀRD��RD� RD�@RDԀRD��RD� RD�@RDՀRD��RD� RD�@RDրRD��RD� RD�@RD׀RD��RD� RD�@RD؀RD��RD� RD�@RDـRD��RD� RD�@RDڀRD��RD� RD�@RDۀRD��RD� RD�@RD܀RD��RD��D�=D݀RD��RD� RD�@RDހRD��RD� RD�@RD߀RD��RD� RD�@RD��RD��RD� RD�@RD�RD��RD� RD�@RD�}D��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD��RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD�ÅD��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�%A�bA��A�bA�bA�oA��A��A� �A��A��A��A� �A� �A�"�A�$�A�$�A�$�A�$�A�"�A�oA���Aҝ�AѼjA�1A���A��yA��mA��HA��yAЃA��`AϓuA���A��A�\)A�ZA�A��+A�A��TA�r�A�t�A��hA��RA�ZA��^A��A��A��FA�XA�  A�-A�G�A��uA��\A��+A��uA���A���A�jA��A��TA�&�A��
A�ĜA���A��hA���A��^A��A�A�G�A�oA�E�A�K�A�p�A~Q�AyƨAt��An{Al�Aj�/Ad�AZ-ATQ�ARANĜALv�AK�AFȴAE�AC��A?�-A<ffA:��A:bA97LA8��A6��A6VA5�#A4�!A49XA3�A2 �A0�A0�9A0z�A/�A.ĜA.~�A.(�A-XA-�A,�A+��A+XA+VA*�/A*n�A(��A(9XA'�-A&��A%A%|�A$��A#�-A#%A"A�A!l�A ��A ĜA A�A�A�AĜAz�A1'A�7A\)A;dAĜAz�A^5AE�AXAVA�yA�DA-A�A�
AC�A33A%Av�A1A&�A�A�yA�jA��AbNA=qA9XA=qA(�A�AS�A�yAI�A��A�hA�A�!A�\AE�A-A�#AS�A�!AI�AbA�#A�7A?}A�!AM�A1A�-A?}A�RA �A�hA`BA
��A
�\A
{A	��A	S�A	l�A	�A	��A	x�A	VAȴA��A9XA�A9XA1'AA�-Al�A�!AZA �A�wA&�A�A��A+A+A ��@�\)@�5?@�O�@��u@���@���@�;d@�$�@�/@�%@�G�@���@�%@�S�@�n�@���@��P@�t�@�C�@���@�$�@��@��@�P@�@�^@�/@��`@�Q�@� �@���@�ƨ@�t�@���@��@�9X@�F@�K�@�"�@��@�bN@�@�@�E�@���@��@��@���@�/@�ƨ@�V@�5?@��T@��/@���@ۮ@ە�@�|�@ى7@ץ�@�t�@�o@�p�@Ԭ@�(�@ӍP@�S�@��@�=q@���@�x�@�G�@�7L@��@���@�b@�dZ@�;d@�o@��@�=q@ͺ^@�/@̬@��
@�33@�
=@��@ʸR@�v�@�V@�=q@�-@�{@��@Ɂ@�G�@�%@���@ȴ9@ȣ�@�j@�1@Ǿw@�C�@�
=@�ȴ@Ɵ�@Ƈ+@�5?@ź^@�x�@���@�bN@î@�"�@��H@�@�n�@�5?@��#@���@���@��j@��u@�z�@�Q�@�A�@�1'@�1@���@���@��P@�o@�5?@���@��@���@�I�@�ƨ@�C�@���@�hs@��`@��@�1'@���@��@�V@�E�@���@��@��/@� �@��F@�S�@�v�@�{@��@���@��h@�?}@�%@���@���@���@���@��9@��D@�bN@�(�@�  @��F@��P@�\)@�@���@�x�@�O�@��j@��w@��@���@�~�@�J@���@���@�j@�(�@�1@��;@��@���@��@�dZ@�
=@���@�n�@��@�p�@�?}@�V@��j@��@��w@���@���@��P@�|�@�l�@�dZ@�S�@�;d@��y@���@�v�@�M�@�-@��@�@��h@�p�@�X@�/@��@��@���@�Q�@�b@�t�@�$�@���@���@��@��@�Q�@��@�t�@��@�ff@�J@��T@�X@��@�(�@���@��@���@�$�@��@��-@�X@�%@��9@���@�o@���@�^5@�@��@��T@���@���@�&�@���@�bN@���@�+@�o@��y@�ȴ@��R@��!@��!@���@��+@�V@�$�@�@���@��D@�r�@�bN@�Q�@�9X@� �@���@���@�C�@���@��@���@���@���@�~�@�^5@�-@��-@��7@�`B@�G�@��@�Ĝ@���@�bN@��@��@�S�@�33@��@�v�@�n�@�ff@�=q@��^@�O�@��@���@�Z@��
@��@�S�@�;d@�+@��@�
=@��@���@�=q@�{@��@��#@�@���@�X@��@��9@��u@��D@��@�A�@+@~��@~��@~��@~�R@}��@|�@|Z@|9X@|9X@|I�@|9X@|(�@{ƨ@{"�@z�!@z��@zM�@y�7@y%@x��@xQ�@w��@w+@v$�@u`B@u�@t�j@t�D@sƨ@r�@q��@qx�@q&�@p�`@p�9@p�@pA�@pb@o�@o��@o�@o|�@o
=@nȴ@n@m/@l��@l�@l(�@k�
@k�@k@j�@i�^@i�7@iG�@i&�@i�@h�`@h��@h�@hQ�@h  @f�R@f5?@f5?@e�@d��@d�D@d1@c��@c�@co@bn�@bJ@a�@a�7@aX@a7L@a�@a%@`��@`r�@`A�@_�@_
=@^ȴ@^ff@]�@]/@\1@[�F@[��@[C�@Z�@Y��@YG�@X�`@XĜ@X�@XbN@XbN@XbN@XQ�@X1'@X �@W�@W\)@V��@U��@U�h@U?}@T�/@T(�@S�
@S�F@S��@SdZ@SdZ@SC�@R�@R^5@R-@Q��@P��@P1'@P �@P �@O�;@O|�@O+@O�@N��@N�y@N�y@N�@Nȴ@N��@Nv�@Nv�@Nff@NV@N5?@M�T@Mp�@L�@L(�@Kƨ@KdZ@K"�@J�!@J�!@J~�@JM�@I�@I��@I��@I�7@IX@I%@G�;@F�+@F$�@E@E��@E�h@E�@E?}@D�@D9X@Cƨ@C@BM�@A�^@A�7@Ahs@AG�@A�@@��@@��@@��@@b@?\)@>v�@>E�@>$�@=�@=@=��@=�@=p�@=O�@=V@<z�@;�m@;��@;�@;C�@;o@:�H@:~�@9�#@9�@8Q�@7�;@7��@7K�@6�y@6V@5�h@4��@4�@3�F@3C�@2��@2n�@1��@1�7@0�`@0�@0bN@0Q�@0b@/�@/�@/��@/K�@/+@.��@.$�@-/@,��@,��@,I�@,(�@,(�@,1@+�
@+�F@+��@+t�@+C�@+o@*�!@*~�@*-@(Ĝ@(Q�@(1'@(1'@( �@'�@'�;@'�P@&��@&�@&�R@&��@&�+@&V@&$�@%��@%�h@%p�@$��@$9X@#��@#��@#�@#�@#C�@#@"��@"�\@"^5@"M�@"-@"-@"=q@"=q@"�@!��@!�@!X@!�@!�@!�@!�@!%@ �`@ �`@ �`@ Ĝ@ �9@ �u@ �@ b@�@�;@�@�P@l�@\)@l�@\)@;d@
=@�y@ȴ@�+@v�@v�@ff@ff@V@5?@{@@��@�@O�@�@�/@�/@�/@�/@�@I�@(�@1@��@��@��@�m@�F@dZ@C�@"�@@�H@�!@J@��@�^@�7@x�@X@X@G�@7L@&�@&�@�@��@��@��@�@1'@��@�w@��@;d@�@�@��@ff@{@@`B@V@�@�@z�@j@1@��@33@o@�@��@�!@^5@=q@�@�@J@��@�@��@G�@�@�`@��@�9@��@��@�u@�@b@��@�P@\)@;d@+@�y@��@E�@$�@@@@��@��@`B@/@��@��@�D@Z@�m@�F@��@��@��@��@��@�@t�@S�@33@o111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�%A�bA��A�bA�bA�oA��A��A� �A��A��A��A� �A� �A�"�A�$�A�$�A�$�A�$�A�"�A�oA���Aҝ�AѼjA�1A���A��yA��mA��HA��yAЃA��`AϓuA���A��A�\)A�ZA�A��+A�A��TA�r�A�t�A��hA��RA�ZA��^A��A��A��FA�XA�  A�-A�G�A��uA��\A��+A��uA���A���A�jA��A��TA�&�A��
A�ĜA���A��hA���A��^A��A�A�G�A�oA�E�A�K�A�p�A~Q�AyƨAt��An{Al�Aj�/Ad�AZ-ATQ�ARANĜALv�AK�AFȴAE�AC��A?�-A<ffA:��A:bA97LA8��A6��A6VA5�#A4�!A49XA3�A2 �A0�A0�9A0z�A/�A.ĜA.~�A.(�A-XA-�A,�A+��A+XA+VA*�/A*n�A(��A(9XA'�-A&��A%A%|�A$��A#�-A#%A"A�A!l�A ��A ĜA A�A�A�AĜAz�A1'A�7A\)A;dAĜAz�A^5AE�AXAVA�yA�DA-A�A�
AC�A33A%Av�A1A&�A�A�yA�jA��AbNA=qA9XA=qA(�A�AS�A�yAI�A��A�hA�A�!A�\AE�A-A�#AS�A�!AI�AbA�#A�7A?}A�!AM�A1A�-A?}A�RA �A�hA`BA
��A
�\A
{A	��A	S�A	l�A	�A	��A	x�A	VAȴA��A9XA�A9XA1'AA�-Al�A�!AZA �A�wA&�A�A��A+A+A ��@�\)@�5?@�O�@��u@���@���@�;d@�$�@�/@�%@�G�@���@�%@�S�@�n�@���@��P@�t�@�C�@���@�$�@��@��@�P@�@�^@�/@��`@�Q�@� �@���@�ƨ@�t�@���@��@�9X@�F@�K�@�"�@��@�bN@�@�@�E�@���@��@��@���@�/@�ƨ@�V@�5?@��T@��/@���@ۮ@ە�@�|�@ى7@ץ�@�t�@�o@�p�@Ԭ@�(�@ӍP@�S�@��@�=q@���@�x�@�G�@�7L@��@���@�b@�dZ@�;d@�o@��@�=q@ͺ^@�/@̬@��
@�33@�
=@��@ʸR@�v�@�V@�=q@�-@�{@��@Ɂ@�G�@�%@���@ȴ9@ȣ�@�j@�1@Ǿw@�C�@�
=@�ȴ@Ɵ�@Ƈ+@�5?@ź^@�x�@���@�bN@î@�"�@��H@�@�n�@�5?@��#@���@���@��j@��u@�z�@�Q�@�A�@�1'@�1@���@���@��P@�o@�5?@���@��@���@�I�@�ƨ@�C�@���@�hs@��`@��@�1'@���@��@�V@�E�@���@��@��/@� �@��F@�S�@�v�@�{@��@���@��h@�?}@�%@���@���@���@���@��9@��D@�bN@�(�@�  @��F@��P@�\)@�@���@�x�@�O�@��j@��w@��@���@�~�@�J@���@���@�j@�(�@�1@��;@��@���@��@�dZ@�
=@���@�n�@��@�p�@�?}@�V@��j@��@��w@���@���@��P@�|�@�l�@�dZ@�S�@�;d@��y@���@�v�@�M�@�-@��@�@��h@�p�@�X@�/@��@��@���@�Q�@�b@�t�@�$�@���@���@��@��@�Q�@��@�t�@��@�ff@�J@��T@�X@��@�(�@���@��@���@�$�@��@��-@�X@�%@��9@���@�o@���@�^5@�@��@��T@���@���@�&�@���@�bN@���@�+@�o@��y@�ȴ@��R@��!@��!@���@��+@�V@�$�@�@���@��D@�r�@�bN@�Q�@�9X@� �@���@���@�C�@���@��@���@���@���@�~�@�^5@�-@��-@��7@�`B@�G�@��@�Ĝ@���@�bN@��@��@�S�@�33@��@�v�@�n�@�ff@�=q@��^@�O�@��@���@�Z@��
@��@�S�@�;d@�+@��@�
=@��@���@�=q@�{@��@��#@�@���@�X@��@��9@��u@��D@��@�A�@+@~��@~��@~��@~�R@}��@|�@|Z@|9X@|9X@|I�@|9X@|(�@{ƨ@{"�@z�!@z��@zM�@y�7@y%@x��@xQ�@w��@w+@v$�@u`B@u�@t�j@t�D@sƨ@r�@q��@qx�@q&�@p�`@p�9@p�@pA�@pb@o�@o��@o�@o|�@o
=@nȴ@n@m/@l��@l�@l(�@k�
@k�@k@j�@i�^@i�7@iG�@i&�@i�@h�`@h��@h�@hQ�@h  @f�R@f5?@f5?@e�@d��@d�D@d1@c��@c�@co@bn�@bJ@a�@a�7@aX@a7L@a�@a%@`��@`r�@`A�@_�@_
=@^ȴ@^ff@]�@]/@\1@[�F@[��@[C�@Z�@Y��@YG�@X�`@XĜ@X�@XbN@XbN@XbN@XQ�@X1'@X �@W�@W\)@V��@U��@U�h@U?}@T�/@T(�@S�
@S�F@S��@SdZ@SdZ@SC�@R�@R^5@R-@Q��@P��@P1'@P �@P �@O�;@O|�@O+@O�@N��@N�y@N�y@N�@Nȴ@N��@Nv�@Nv�@Nff@NV@N5?@M�T@Mp�@L�@L(�@Kƨ@KdZ@K"�@J�!@J�!@J~�@JM�@I�@I��@I��@I�7@IX@I%@G�;@F�+@F$�@E@E��@E�h@E�@E?}@D�@D9X@Cƨ@C@BM�@A�^@A�7@Ahs@AG�@A�@@��@@��@@��@@b@?\)@>v�@>E�@>$�@=�@=@=��@=�@=p�@=O�@=V@<z�@;�m@;��@;�@;C�@;o@:�H@:~�@9�#@9�@8Q�@7�;@7��@7K�@6�y@6V@5�h@4��@4�@3�F@3C�@2��@2n�@1��@1�7@0�`@0�@0bN@0Q�@0b@/�@/�@/��@/K�@/+@.��@.$�@-/@,��@,��@,I�@,(�@,(�@,1@+�
@+�F@+��@+t�@+C�@+o@*�!@*~�@*-@(Ĝ@(Q�@(1'@(1'@( �@'�@'�;@'�P@&��@&�@&�R@&��@&�+@&V@&$�@%��@%�h@%p�@$��@$9X@#��@#��@#�@#�@#C�@#@"��@"�\@"^5@"M�@"-@"-@"=q@"=q@"�@!��@!�@!X@!�@!�@!�@!�@!%@ �`@ �`@ �`@ Ĝ@ �9@ �u@ �@ b@�@�;@�@�P@l�@\)@l�@\)@;d@
=@�y@ȴ@�+@v�@v�@ff@ff@V@5?@{@@��@�@O�@�@�/@�/@�/@�/@�@I�@(�@1@��@��@��@�m@�F@dZ@C�@"�@@�H@�!@J@��@�^@�7@x�@X@X@G�@7L@&�@&�@�@��@��@��@�@1'@��@�w@��@;d@�@�@��@ff@{@@`B@V@�@�@z�@j@1@��@33@o@�@��@�!@^5@=q@�@�@J@��@�@��@G�@�@�`@��@�9@��@��@�u@�@b@��@�P@\)@;d@+@�y@��@E�@$�@@@@��@��@`B@/@��@��@�D@Z@�m@�F@��@��@��@��@��@�@t�@S�@33@o111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B	�)B	�#B	�#B	�#B	�#B	�#B	�B	�#B	�B	�B	�#B	�#B	�B	�#B	�B	�B	�B	�B	�#B	�#B	�BB	�TB	�B
<jB
�JB
�B
��B	7BoB(�B2-B_;Bu�B{�Bv�B� B�B�bB�B{B+B�B��B�B"�B5?BP�BYBW
BQ�BJ�B6FB2-B7LBF�B_;BM�B8RB�B�B�#BǮB��B�VBs�B=qB
��B
�B
�;B
�B
�fB
��B
�-B
p�B
?}B	��B	�sB	B	�B	q�B	�B	bNB	�B�B�mB	JB	DB	hB	�B	hB	-B	2-B	0!B	A�B	t�B	�B	�7B	�VB	�VB	�9B	�jB	�}B	�#B	�;B	�B	��B
hB
oB
hB
PB
�B
�B
�B
&�B
%�B
�B
+B
.B
.B
(�B
"�B
.B
49B
5?B
1'B
@�B
;dB
<jB
B�B
A�B
D�B
I�B
L�B
J�B
F�B
O�B
Q�B
Q�B
O�B
M�B
Q�B
ZB
W
B
\)B
aHB
`BB
\)B
`BB
ffB
dZB
cTB
cTB
dZB
_;B
dZB
cTB
^5B
^5B
XB
_;B
^5B
]/B
]/B
]/B
^5B
aHB
aHB
aHB
\)B
^5B
\)B
YB
[#B
[#B
ZB
]/B
_;B
]/B
^5B
[#B
W
B
W
B
XB
ZB
YB
W
B
VB
Q�B
O�B
P�B
L�B
I�B
G�B
A�B
@�B
@�B
=qB
?}B
>wB
;dB
>wB
B�B
D�B
D�B
C�B
?}B
=qB
C�B
D�B
G�B
K�B
K�B
H�B
G�B
F�B
@�B
@�B
?}B
:^B
1'B
#�B
"�B
uB
$�B
�B
bB
\B
VB
�B
\B
!�B
#�B
�B
�B
"�B
%�B
)�B
'�B
 �B
%�B
/B
5?B
49B
2-B
/B
.B
)�B
+B
,B
&�B
%�B
%�B
%�B
"�B
!�B
�B
�B
�B
{B
�B
�B
�B
�B
�B
uB
hB
�B
{B
�B
�B
�B
\B

=B
	7B
B
%B
PB
uB
oB
{B
�B
�B
{B
DB
DB
�B
{B
\B
{B
�B
�B
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
!�B
"�B
"�B
"�B
!�B
!�B
�B
 �B
 �B
 �B
!�B
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
�B
�B
�B
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
�B
�B
�B
�B
�B
�B
�B
�B
uB
hB
�B
�B
�B
�B
{B
{B
�B
�B
�B
{B
�B
�B
�B
�B
�B
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
!�B
!�B
!�B
#�B
"�B
"�B
!�B
!�B
#�B
"�B
"�B
$�B
$�B
#�B
"�B
%�B
(�B
(�B
(�B
(�B
(�B
)�B
(�B
(�B
&�B
'�B
)�B
)�B
(�B
(�B
(�B
)�B
)�B
+B
+B
+B
)�B
(�B
'�B
'�B
$�B
"�B
+B
-B
+B
+B
,B
,B
,B
+B
-B
.B
/B
,B
+B
/B
-B
-B
0!B
0!B
33B
33B
2-B
33B
2-B
0!B
0!B
49B
7LB
6FB
9XB
9XB
8RB
7LB
6FB
6FB
7LB
7LB
6FB
:^B
:^B
;dB
<jB
<jB
<jB
<jB
;dB
;dB
:^B
9XB
7LB
=qB
>wB
>wB
>wB
>wB
=qB
<jB
<jB
=qB
>wB
?}B
@�B
@�B
@�B
@�B
?}B
?}B
>wB
@�B
@�B
@�B
@�B
?}B
@�B
@�B
?}B
?}B
@�B
B�B
A�B
A�B
D�B
C�B
B�B
A�B
B�B
D�B
C�B
C�B
C�B
F�B
F�B
G�B
H�B
H�B
H�B
G�B
F�B
F�B
H�B
H�B
I�B
I�B
H�B
G�B
H�B
H�B
J�B
K�B
J�B
I�B
G�B
L�B
M�B
L�B
K�B
I�B
J�B
L�B
N�B
N�B
N�B
N�B
N�B
M�B
L�B
M�B
N�B
M�B
L�B
M�B
O�B
N�B
N�B
N�B
M�B
O�B
Q�B
Q�B
Q�B
P�B
O�B
P�B
R�B
S�B
T�B
T�B
T�B
T�B
VB
VB
VB
VB
T�B
T�B
T�B
S�B
S�B
W
B
W
B
VB
W
B
W
B
W
B
W
B
YB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
YB
XB
W
B
YB
[#B
YB
YB
[#B
[#B
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
]/B
]/B
]/B
\)B
\)B
^5B
]/B
]/B
]/B
\)B
_;B
`BB
_;B
_;B
]/B
`BB
`BB
aHB
aHB
bNB
bNB
bNB
bNB
aHB
aHB
aHB
_;B
`BB
^5B
aHB
aHB
aHB
`BB
aHB
bNB
bNB
bNB
bNB
bNB
aHB
aHB
aHB
aHB
aHB
bNB
dZB
dZB
dZB
cTB
dZB
e`B
e`B
gmB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
ffB
e`B
e`B
e`B
ffB
ffB
gmB
gmB
hsB
hsB
hsB
gmB
hsB
iyB
hsB
hsB
ffB
e`B
e`B
jB
jB
k�B
k�B
k�B
jB
jB
iyB
iyB
iyB
iyB
k�B
l�B
l�B
l�B
m�B
l�B
m�B
l�B
k�B
k�B
l�B
o�B
o�B
o�B
o�B
p�B
p�B
p�B
p�B
o�B
n�B
o�B
p�B
q�B
p�B
p�B
p�B
p�B
n�B
o�B
o�B
p�B
q�B
q�B
p�B
p�B
p�B
p�B
q�B
s�B
s�B
t�B
t�B
t�B
t�B
t�B
v�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
u�B
v�B
y�B
z�B
z�B
z�B
{�B
z�B
z�B
{�B
{�B
z�B
z�B
z�B
z�B
z�B
z�B
x�B
{�B
}�B
}�B
}�B
|�B
|�B
|�B
{�B
|�B
}�B
}�B
}�B
|�B
|�B
|�B
|�B
}�B
{�B
|�B
~�B
~�B
� B
� B
� B
~�B
� B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
� B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�%B
�%B
�%B
�B
�B
�%B
�+B
�+B
�1B
�+B
�+B
�%B
�%B
�+B
�+B
�+B
�+B
�+B
�%B
�1B
�7B
�1B
�7B
�7B
�7B
�7B
�7B
�7B
�7B
�7B
�7B
�7B
�7B
�7B
�1B
�1B
�=B
�7B
�7B
�=B
�=B
�7B
�=B
�=B
�=B
�=B
�DB
�JB
�JB
�JB
�JB
�DB
�DB
�PB
�PB
�VB
�VB
�VB
�VB
�VB
�\B
�\B
�\B
�\B
�\B
�VB
�VB
�\B
�bB
�bB
�bB
�hB
�hB
�hB
�bB
�\B
�bB
�hB
�hB
�oB
�oB
�hB
�hB
�hB
�uB
�uB
�uB
�uB
�uB
�oB
�oB
�uB
�uB
�uB
�uB
�uB
�uB
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
��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B	�)B	�#B	�#B	�#B	�#B	�#B	�B	�#B	�B	�B	�#B	�#B	�B	�#B	�B	�B	�B	�B	�#B	�#B	�BB	�TB	�B
<jB
�JB
�B
��B	7BoB(�B2-B_;Bu�B{�Bv�B� B�B�bB�B{B+B�B��B�B"�B5?BP�BYBW
BQ�BJ�B6FB2-B7LBF�B_;BM�B8RB�B�B�#BǮB��B�VBs�B=qB
��B
�B
�;B
�B
�fB
��B
�-B
p�B
?}B	��B	�sB	B	�B	q�B	�B	bNB	�B�B�mB	JB	DB	hB	�B	hB	-B	2-B	0!B	A�B	t�B	�B	�7B	�VB	�VB	�9B	�jB	�}B	�#B	�;B	�B	��B
hB
oB
hB
PB
�B
�B
�B
&�B
%�B
�B
+B
.B
.B
(�B
"�B
.B
49B
5?B
1'B
@�B
;dB
<jB
B�B
A�B
D�B
I�B
L�B
J�B
F�B
O�B
Q�B
Q�B
O�B
M�B
Q�B
ZB
W
B
\)B
aHB
`BB
\)B
`BB
ffB
dZB
cTB
cTB
dZB
_;B
dZB
cTB
^5B
^5B
XB
_;B
^5B
]/B
]/B
]/B
^5B
aHB
aHB
aHB
\)B
^5B
\)B
YB
[#B
[#B
ZB
]/B
_;B
]/B
^5B
[#B
W
B
W
B
XB
ZB
YB
W
B
VB
Q�B
O�B
P�B
L�B
I�B
G�B
A�B
@�B
@�B
=qB
?}B
>wB
;dB
>wB
B�B
D�B
D�B
C�B
?}B
=qB
C�B
D�B
G�B
K�B
K�B
H�B
G�B
F�B
@�B
@�B
?}B
:^B
1'B
#�B
"�B
uB
$�B
�B
bB
\B
VB
�B
\B
!�B
#�B
�B
�B
"�B
%�B
)�B
'�B
 �B
%�B
/B
5?B
49B
2-B
/B
.B
)�B
+B
,B
&�B
%�B
%�B
%�B
"�B
!�B
�B
�B
�B
{B
�B
�B
�B
�B
�B
uB
hB
�B
{B
�B
�B
�B
\B

=B
	7B
B
%B
PB
uB
oB
{B
�B
�B
{B
DB
DB
�B
{B
\B
{B
�B
�B
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
!�B
"�B
"�B
"�B
!�B
!�B
�B
 �B
 �B
 �B
!�B
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
�B
�B
�B
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
�B
�B
�B
�B
�B
�B
�B
�B
uB
hB
�B
�B
�B
�B
{B
{B
�B
�B
�B
{B
�B
�B
�B
�B
�B
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
!�B
!�B
!�B
#�B
"�B
"�B
!�B
!�B
#�B
"�B
"�B
$�B
$�B
#�B
"�B
%�B
(�B
(�B
(�B
(�B
(�B
)�B
(�B
(�B
&�B
'�B
)�B
)�B
(�B
(�B
(�B
)�B
)�B
+B
+B
+B
)�B
(�B
'�B
'�B
$�B
"�B
+B
-B
+B
+B
,B
,B
,B
+B
-B
.B
/B
,B
+B
/B
-B
-B
0!B
0!B
33B
33B
2-B
33B
2-B
0!B
0!B
49B
7LB
6FB
9XB
9XB
8RB
7LB
6FB
6FB
7LB
7LB
6FB
:^B
:^B
;dB
<jB
<jB
<jB
<jB
;dB
;dB
:^B
9XB
7LB
=qB
>wB
>wB
>wB
>wB
=qB
<jB
<jB
=qB
>wB
?}B
@�B
@�B
@�B
@�B
?}B
?}B
>wB
@�B
@�B
@�B
@�B
?}B
@�B
@�B
?}B
?}B
@�B
B�B
A�B
A�B
D�B
C�B
B�B
A�B
B�B
D�B
C�B
C�B
C�B
F�B
F�B
G�B
H�B
H�B
H�B
G�B
F�B
F�B
H�B
H�B
I�B
I�B
H�B
G�B
H�B
H�B
J�B
K�B
J�B
I�B
G�B
L�B
M�B
L�B
K�B
I�B
J�B
L�B
N�B
N�B
N�B
N�B
N�B
M�B
L�B
M�B
N�B
M�B
L�B
M�B
O�B
N�B
N�B
N�B
M�B
O�B
Q�B
Q�B
Q�B
P�B
O�B
P�B
R�B
S�B
T�B
T�B
T�B
T�B
VB
VB
VB
VB
T�B
T�B
T�B
S�B
S�B
W
B
W
B
VB
W
B
W
B
W
B
W
B
YB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
YB
XB
W
B
YB
[#B
YB
YB
[#B
[#B
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
]/B
]/B
]/B
\)B
\)B
^5B
]/B
]/B
]/B
\)B
_;B
`BB
_;B
_;B
]/B
`BB
`BB
aHB
aHB
bNB
bNB
bNB
bNB
aHB
aHB
aHB
_;B
`BB
^5B
aHB
aHB
aHB
`BB
aHB
bNB
bNB
bNB
bNB
bNB
aHB
aHB
aHB
aHB
aHB
bNB
dZB
dZB
dZB
cTB
dZB
e`B
e`B
gmB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
ffB
e`B
e`B
e`B
ffB
ffB
gmB
gmB
hsB
hsB
hsB
gmB
hsB
iyB
hsB
hsB
ffB
e`B
e`B
jB
jB
k�B
k�B
k�B
jB
jB
iyB
iyB
iyB
iyB
k�B
l�B
l�B
l�B
m�B
l�B
m�B
l�B
k�B
k�B
l�B
o�B
o�B
o�B
o�B
p�B
p�B
p�B
p�B
o�B
n�B
o�B
p�B
q�B
p�B
p�B
p�B
p�B
n�B
o�B
o�B
p�B
q�B
q�B
p�B
p�B
p�B
p�B
q�B
s�B
s�B
t�B
t�B
t�B
t�B
t�B
v�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
u�B
v�B
y�B
z�B
z�B
z�B
{�B
z�B
z�B
{�B
{�B
z�B
z�B
z�B
z�B
z�B
z�B
x�B
{�B
}�B
}�B
}�B
|�B
|�B
|�B
{�B
|�B
}�B
}�B
}�B
|�B
|�B
|�B
|�B
}�B
{�B
|�B
~�B
~�B
� B
� B
� B
~�B
� B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
� B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�%B
�%B
�%B
�B
�B
�%B
�+B
�+B
�1B
�+B
�+B
�%B
�%B
�+B
�+B
�+B
�+B
�+B
�%B
�1B
�7B
�1B
�7B
�7B
�7B
�7B
�7B
�7B
�7B
�7B
�7B
�7B
�7B
�7B
�1B
�1B
�=B
�7B
�7B
�=B
�=B
�7B
�=B
�=B
�=B
�=B
�DB
�JB
�JB
�JB
�JB
�DB
�DB
�PB
�PB
�VB
�VB
�VB
�VB
�VB
�\B
�\B
�\B
�\B
�\B
�VB
�VB
�\B
�bB
�bB
�bB
�hB
�hB
�hB
�bB
�\B
�bB
�hB
�hB
�oB
�oB
�hB
�hB
�hB
�uB
�uB
�uB
�uB
�uB
�oB
�oB
�uB
�uB
�uB
�uB
�uB
�uB
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
��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.01 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted during real time processing based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       20230210080132                              AO  ARCAADJP                                                                    20230210080132    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20230210080132  QCP$                G�O�G�O�G�O�1F83E           AO  ARGQQCPL                                                                    20230210080132  QCF$                G�O�G�O�G�O�0               