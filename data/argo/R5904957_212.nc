CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-24T14:08:44Z creation      
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
resolution        =���   axis      Z        �  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ?�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  A�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  H$   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  I�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  PP   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  V�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  X|   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  _   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  `�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  g0   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  m�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  o\   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  u�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  w�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ~   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ~@   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �@   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �@   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �@   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �l   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �p   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �t   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �x   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �|   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20181024140844  20181024140844  5904957 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6560                            2B  A   APEX                            7471                            062512                          846 @�䤒�D1   @��/h^2@5!$�/�d�-1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �A   A   A   @   @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A���B   B  B  B  B   B'��B0  B8  B@  BH  BP  BX  B`  Bh  Bp  BxffB�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  B���B���B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C�C  C  C  C  C   C"  C$  C&  C(  C*  C,�C.�C0  C2  C4  C6�C8�C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C[�fC^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv�Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C��C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C��C�  C��3C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C��3C�  C�  C�  C�  C�  C�  C�  C��3C��3C��3D y�D  D� D  Dy�D  D� D  D� D  D�fDfD�fDfD� D  D� D	  D	� D
  D
� D  D� DfD� D  Dy�D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D  D� DfD� D��Dy�D��D � D!  D!� D"  D"� D"��D#� D$fD$� D%  D%� D&  D&� D'  D'� D(  D(�fD)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D2��D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� Dy�\D�/\D��H1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @(��@�z�@�z�A=qA"=qAB=qAb=qA��A��A��A��A��A��A��A��B �\B�\B�\B�\B �\B((�B0�\B8�\B@�\BH�\BP�\BX�\B`�\Bh�\Bp�\Bx��B�z�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�{B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�z�B�z�B�G�B�{B�{B�G�B�G�B�G�B�G�B�G�B�G�C #�C#�C#�C#�C#�C
#�C#�C#�C#�C#�C#�C=qC#�C#�C#�C#�C #�C"#�C$#�C&#�C(#�C*#�C,=qC.=qC0#�C2#�C4#�C6=qC8=qC:#�C<#�C>#�C@#�CB#�CD#�CF#�CH#�CJ#�CL#�CN#�CP#�CR#�CT#�CV#�CX#�CZ#�C\
=C^#�C`#�Cb#�Cd#�Cf#�Ch#�Cj#�Cl#�Cn#�Cp#�Cr#�Ct#�Cv=qCx#�Cz#�C|#�C~#�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�C�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�C��C��C��C��C��C��C�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�C��C�C��C��C��C��C��C��C��C��C��C��C��C�C��C��C��C��C��C��C��C�C�D �D ��D�D��D�D��D�D��D�D��D�D�\D\D�\D\D��D�D��D	�D	��D
�D
��D�D��D\D��D�D��D�D�\D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D\D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$\D$��D%�D%��D&�D&��D'�D'��D(�D(�\D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��Dy�RD�3�D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��TA��`A��yA��yA��yA��A��A��yA��/A���A���AǮA�E�A��mA�jA�K�A�K�A�G�A�A�A�9XA�oA��A���A���AŲ-Aš�Ař�Aŕ�Aŏ\AŃA�~�A�v�A�x�A�I�A�I�A�K�A�7LA�1'A���AċDA�"�A��A�p�A�S�A�G�A�=qA��A��uA���A�VA���A���A��jA��7A�XA�A�t�A�5?A�K�A�-A���A���A��A�ĜA��+A�Q�A��`A��A��/A��A���A�5?A�A�A��A�ƨA��hA�ĜA��^A���A��
A�$�A��A���A���A��A���A�33A��;A��FA�ȴA��A���A�dZA��A�XA��A���A��A�\)A�A��hA�33A�+A��PA�(�A��HA�~�A��/A�G�A~E�A|ffAz��Ay�#AyXAxQ�AwO�Av{ArȴAm��Aj�\Af=qAb�A_K�A^�A\�\AYXAV��AVv�AT�`AS7LAQ��AQAOhsAL��AJ�!AIdZAH��AG+AD �ABZA?l�A=�A;VA:~�A9�A8{A5�A5XA5�A4~�A3�^A1S�A/�7A/33A.�HA.��A.�A.I�A-�A-dZA,^5A*�A'�^A&^5A%�TA%x�A%%A$�RA"5?A bNA|�A
=AffA�9AXAjA�yA"�A5?A+A�;AA�RAA�A�-A�`Av�A�A��A�A
��A
A�A
�+A
��A
�uA
r�A
Q�A
A	�wA	dZA	�A��A�!A�!AbA;dAM�AA��A�9A��A�DA��A �+A $�@�;d@��-@���@�l�@���@��/@�w@�?}@�Z@� �@���@�Ĝ@�x�@�P@�^5@�^@ᙚ@�`B@߾w@޸R@�n�@�{@���@ݲ-@�hs@�O�@��`@���@�;d@��#@ؼj@�j@���@ؼj@�r�@�z�@�j@���@ָR@�{@�@�`B@�b@�l�@�S�@Ұ!@��@Ο�@Ͳ-@�X@��@̴9@��/@��/@�Q�@�o@��@�?}@Ȭ@Ǖ�@��@Ɨ�@�~�@�V@��T@�7L@�I�@��
@�t�@�"�@�@�^5@�=q@��^@�`B@���@��@�v�@�@���@��7@��@��D@��@�o@���@��7@���@�{@�+@�S�@���@��w@�9X@��h@�v�@�X@�~�@��@��F@�`B@���@�t�@��@��P@��`@�`B@��@��@�b@�+@���@�ȴ@�=q@�$�@���@�bN@�1'@�1'@�1'@��@��F@���@��@�\)@�;d@��@�~�@�V@�$�@���@���@��@�/@���@��@�Z@�(�@�  @��@�o@��y@�v�@�{@���@��@�G�@�?}@�%@�b@�dZ@�;d@�33@�K�@�K�@�S�@�K�@�@��R@���@��@�V@�%@�bN@��F@���@�ƨ@���@��P@�t�@�l�@�\)@�;d@�33@�+@�+@��@���@��y@�ȴ@��+@�M�@�J@���@��h@�x�@�X@��@�V@���@���@�bN@���@�dZ@�33@�ȴ@�=q@��@�r�@�1'@�!�@}��@jW�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��TA��`A��yA��yA��yA��A��A��yA��/A���A���AǮA�E�A��mA�jA�K�A�K�A�G�A�A�A�9XA�oA��A���A���AŲ-Aš�Ař�Aŕ�Aŏ\AŃA�~�A�v�A�x�A�I�A�I�A�K�A�7LA�1'A���AċDA�"�A��A�p�A�S�A�G�A�=qA��A��uA���A�VA���A���A��jA��7A�XA�A�t�A�5?A�K�A�-A���A���A��A�ĜA��+A�Q�A��`A��A��/A��A���A�5?A�A�A��A�ƨA��hA�ĜA��^A���A��
A�$�A��A���A���A��A���A�33A��;A��FA�ȴA��A���A�dZA��A�XA��A���A��A�\)A�A��hA�33A�+A��PA�(�A��HA�~�A��/A�G�A~E�A|ffAz��Ay�#AyXAxQ�AwO�Av{ArȴAm��Aj�\Af=qAb�A_K�A^�A\�\AYXAV��AVv�AT�`AS7LAQ��AQAOhsAL��AJ�!AIdZAH��AG+AD �ABZA?l�A=�A;VA:~�A9�A8{A5�A5XA5�A4~�A3�^A1S�A/�7A/33A.�HA.��A.�A.I�A-�A-dZA,^5A*�A'�^A&^5A%�TA%x�A%%A$�RA"5?A bNA|�A
=AffA�9AXAjA�yA"�A5?A+A�;AA�RAA�A�-A�`Av�A�A��A�A
��A
A�A
�+A
��A
�uA
r�A
Q�A
A	�wA	dZA	�A��A�!A�!AbA;dAM�AA��A�9A��A�DA��A �+A $�@�;d@��-@���@�l�@���@��/@�w@�?}@�Z@� �@���@�Ĝ@�x�@�P@�^5@�^@ᙚ@�`B@߾w@޸R@�n�@�{@���@ݲ-@�hs@�O�@��`@���@�;d@��#@ؼj@�j@���@ؼj@�r�@�z�@�j@���@ָR@�{@�@�`B@�b@�l�@�S�@Ұ!@��@Ο�@Ͳ-@�X@��@̴9@��/@��/@�Q�@�o@��@�?}@Ȭ@Ǖ�@��@Ɨ�@�~�@�V@��T@�7L@�I�@��
@�t�@�"�@�@�^5@�=q@��^@�`B@���@��@�v�@�@���@��7@��@��D@��@�o@���@��7@���@�{@�+@�S�@���@��w@�9X@��h@�v�@�X@�~�@��@��F@�`B@���@�t�@��@��P@��`@�`B@��@��@�b@�+@���@�ȴ@�=q@�$�@���@�bN@�1'@�1'@�1'@��@��F@���@��@�\)@�;d@��@�~�@�V@�$�@���@���@��@�/@���@��@�Z@�(�@�  @��@�o@��y@�v�@�{@���@��@�G�@�?}@�%@�b@�dZ@�;d@�33@�K�@�K�@�S�@�K�@�@��R@���@��@�V@�%@�bN@��F@���@�ƨ@���@��P@�t�@�l�@�\)@�;d@�33@�+@�+@��@���@��y@�ȴ@��+@�M�@�J@���@��h@�x�@�X@��@�V@���@���@�bN@���@�dZ@�33@�ȴ@�=q@��@�r�@�1'@�!�@}��@jW�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�B�B�B�B�B�B�B�B�B��B��B��BB
=B�B�B �B!�B$�B+BC�BXBdZBjBp�Bv�Bx�Bz�B}�B�B�B�VB��B��B�B�B�B�9B�'B�B��B�3B�}B��B�RB�FB�'B�B��B��B��B��B��B��B��B��B��B��B��B�'B�?B�XB�jB�qB�wB�qB�RB�'B��B��B�{B�Bt�BhsB^5BP�B<jB$�B�B\B%B��B�B�BB��BƨB�qB�RB��B��B�PB�Bu�BcTBL�B9XB#�BB
��B
��B
��B
��B
��B
�ZB
��B
ȴB
�wB
��B
��B
�%B
x�B
jB
cTB
]/B
VB
K�B
>wB
�B	�yB	ǮB	��B	�DB	~�B	u�B	hsB	P�B	?}B	;dB	6FB	/B	'�B	#�B	�B	\B	+B	B��B��B�B�HB�
B��B��BɺBƨB��B�wB�qB�jB�^B�FB�'B�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B�{B�bB�VB�JB�7B�%B� Bz�Bs�Bl�BhsBcTB_;B\)BZBYBXBVBR�BN�BK�BK�BN�BR�BZB]/B^5B`BBaHBdZBffBiyBl�Bp�Bq�Bq�Bu�Bw�By�By�Bw�Bt�Bs�Bq�Bn�Bl�Bk�Be`B_;BW
BL�BI�BT�BYB[#BgmBm�Bk�BgmBhsBk�BffBe`BgmBr�Bw�By�By�Bz�Bz�B{�B}�B�1B�=B�DB�JB�=B�DB�oB��B��B��B��B��B��B��B��B��B��B��B��B��B�B�'B�?B�3B�'B�'B�!B�-B�dB�jB��BBÖBÖBƨBȴBɺBɺBɺB��B��B�B�B�/B�;B�NB�ZB�`B�mB�sB�B�B�B�B��B��B��B��B��B	B	B	B	+B	B�mB�B��B��B�mB�5B�B�HB��B��B	B	VB	�B	�B	!�B	"�B	/B	;dB	<jB	=qB	@�B	A�B	C�B	G�B	I�B	J�B	L�B	Q�B	S�B	S�B	S�B	W
B	YB	YB	ZB	[#B	\)B	]/B	bNB	ffB	hsB	iyB	jB	jB	jB	k�B	r�B	s�B	s�B	t�B	u�B	v�B	v�B	x�B	z�B	|�B	�B	�B	�B	�B	�%B	�+B	�+B	�1B	�=B	�DB	�DB	�JB	�VB	�\B	�\B	�VB	�JB	�JB	�DB	�JB	�hB	�oB	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�'B	�-B	�9B	�FB	�FB	�?B	�?B	�?B	��B
�B
&�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B�B�B�B�B�B�B�B�B�B��B��B��BB
=B�B�B �B!�B$�B+BC�BXBdZBjBp�Bv�Bx�Bz�B}�B�B�B�VB��B��B�B�B�B�9B�'B�B��B�3B�}B��B�RB�FB�'B�B��B��B��B��B��B��B��B��B��B��B��B�'B�?B�XB�jB�qB�wB�qB�RB�'B��B��B�{B�Bt�BhsB^5BP�B<jB$�B�B\B%B��B�B�BB��BƨB�qB�RB��B��B�PB�Bu�BcTBL�B9XB#�BB
��B
��B
��B
��B
��B
�ZB
��B
ȴB
�wB
��B
��B
�%B
x�B
jB
cTB
]/B
VB
K�B
>wB
�B	�yB	ǮB	��B	�DB	~�B	u�B	hsB	P�B	?}B	;dB	6FB	/B	'�B	#�B	�B	\B	+B	B��B��B�B�HB�
B��B��BɺBƨB��B�wB�qB�jB�^B�FB�'B�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B�{B�bB�VB�JB�7B�%B� Bz�Bs�Bl�BhsBcTB_;B\)BZBYBXBVBR�BN�BK�BK�BN�BR�BZB]/B^5B`BBaHBdZBffBiyBl�Bp�Bq�Bq�Bu�Bw�By�By�Bw�Bt�Bs�Bq�Bn�Bl�Bk�Be`B_;BW
BL�BI�BT�BYB[#BgmBm�Bk�BgmBhsBk�BffBe`BgmBr�Bw�By�By�Bz�Bz�B{�B}�B�1B�=B�DB�JB�=B�DB�oB��B��B��B��B��B��B��B��B��B��B��B��B��B�B�'B�?B�3B�'B�'B�!B�-B�dB�jB��BBÖBÖBƨBȴBɺBɺBɺB��B��B�B�B�/B�;B�NB�ZB�`B�mB�sB�B�B�B�B��B��B��B��B��B	B	B	B	+B	B�mB�B��B��B�mB�5B�B�HB��B��B	B	VB	�B	�B	!�B	"�B	/B	;dB	<jB	=qB	@�B	A�B	C�B	G�B	I�B	J�B	L�B	Q�B	S�B	S�B	S�B	W
B	YB	YB	ZB	[#B	\)B	]/B	bNB	ffB	hsB	iyB	jB	jB	jB	k�B	r�B	s�B	s�B	t�B	u�B	v�B	v�B	x�B	z�B	|�B	�B	�B	�B	�B	�%B	�+B	�+B	�1B	�=B	�DB	�DB	�JB	�VB	�\B	�\B	�VB	�JB	�JB	�DB	�JB	�hB	�oB	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�'B	�-B	�9B	�FB	�FB	�?B	�?B	�?B	��B
�B
&�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.14 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181024140844                              AO  ARCAADJP                                                                    20181024140844    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181024140844  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181024140844  QCF$                G�O�G�O�G�O�0               