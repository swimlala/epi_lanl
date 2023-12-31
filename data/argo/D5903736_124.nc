CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-09-13T19:16:14Z AOML 3.0 creation; 2016-05-31T19:14:45Z UW 3.1 conversion     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7    PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7X   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8<   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8\   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~       axis      T           8`   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8h   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~            8l   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8t   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8|   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    K�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  U|   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ]p   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  _p   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    gd   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  id   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  qX   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    yL   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {L   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �@   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �@   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �4   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �d   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �d   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �d   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �d   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20150913191614  20160531121445  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               |A   AO  4051_7090_124                   2C  D   APEX                            5368                            041511                          846 @�orX��1   @�o�۠g@3�ě��T�deO�;dZ1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    |A   A   A   @�33@�  A   A!��A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   BffB  B  B   B(  B0  B8  B@  BH  BP  BXffB`  Bh  Bp  Bx  B�  B�  B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�ffB���B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9y�D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dtl�Dx�3D�	�D�I�D�i�D�ɚD� D�S3D�� D�� D��fD�I�D�vfD��fD�	�D�C3Dډ�D��fD��D�,�D�fD��311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@�Q�A (�A!A@(�A`(�A�{A�{A�{A�{A�{A�{A�{A�{B 
=Bp�B
=B
=B 
=B(
=B0
=B8
=B@
=BH
=BP
=BXp�B`
=Bh
=Bp
=Bx
=B�B�B�B�B�8RB�8RB�B�B�B�B�B�B�B�B�B�8RB�B�B�B�B�B�B�B�B�B�B�8RB�k�B���B�B�B�C �C�C�C�C�C
�C�C�C�C�C�C�C�C�C�C�C �C"�C$�C&�C(�C*�C,�C.�C0�C2�C4�C6�C8�C:�C<�C>�C@�CB�CD�CF�CH�CJ�CL�CN�CP�CR�CT�CV�CX�CZ�C\�C^�C`�Cb�Cd�Cf�Ch�Cj�Cl�Cn�Cp�Cr�Ct�Cv�Cx�Cz�C|�C~�C�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�C�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HD  �D ��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D	 �D	��D
 �D
��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D  �D ��D! �D!��D" �D"��D# �D#��D$ �D$��D% �D%��D& �D&��D' �D'��D( �D(��D) �D)��D* �D*��D+ �D+��D, �D,��D- �D-��D. �D.��D/ �D/��D0 �D0��D1 �D1��D2 �D2��D3 �D3��D4 �D4��D5 �D5��D6 �D6��D7 �D7��D8 �D8��D9 �D9z>D: �D:��D; �D;��D< �D<��D= �D=��D> �D>��D? �D?��D@ �D@��DA �DA��DB �DB��DC �DC��DD �DD��DE �DE��DF �DF��DG �DG��DH �DH��DI �DI��DJ �DJ��DK �DK��DL �DL��DM �DM��DN �DN��DO �DO��DP �DP��DQ �DQ��DR �DR��DS �DS��DT �DT��DU �DU��DV �DV��DW �DW��DX �DX��DY �DY��DZ �DZ��D[ �D[��D\ �D\��D] �D]��D^ �D^��D_ �D_��D` �D`��Da �Da��Db �Db��Dc �Dc��Dd �Dd��De �De��Df �Df��Dg �Dg��Dh �Dh��Di �Di��Dj �Dj��Dk �Dk��Dl �Dl��Dm �Dm��Dn �Dn��Do �Do��Dp �Dp��Dq �Dq��Dr �Dr��Ds �Ds��Dt �DtmqDx��D�	�D�I�D�i�D���D�RD�S�D��RD��RD���D�I�D�v�D�ָD�	�D�C�Dډ�D�ƸD�D�-D�D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�ȴA���A���A���A���A��A�bNA���A�wA�DA�A�Q�A�JA�-A��A�Q�A�33A���A�S�A���A�
=A䝲A���A�VA���Aݛ�A��`A���Aٙ�A�;dAכ�A�dZA�&�A��#Aִ9A�1'AՋDAӲ-A��A�bNAѣ�AжFA�jAζFA͙�A�1A� �A��HA�%A���A��HA�$�AƉ7A���A��mA�hsA�"�A�A�bA���A���A���A��7A�A���A�=qA�K�A�dZA��HA�ZA�1A�%A��A�+A�A�9XA���A�Q�A�|�A�z�A�?}A��DA��A�&�A��mA��9A��A��jA��DA���A�33A�z�A�1A��mA���A���A��HA�dZA��DA��A��A��
A�A��A��jA��RA��
A�ƨA�ffA��
A��A�+A��RA��A��A�33A�5?A��
A�  A�r�A��A�-A��A��#A��A�ƨA��!A�A��!A�l�A�bNA�n�A|ZAw��Av  Arz�Ao/Amt�Ai&�Ae;dAcC�Aa�FA^ �A\�A[hsAY�^AX��AU��AT�HAT1AR�9AQ�AO��AN5?AM�AKC�AI�;AHffAG?}AE`BAB��AA33A>�A;��A:{A61'A41'A2�jA2bA1l�A/&�A.bA-;dA,9XA+�A*ffA(�A'�wA'VA&bNA%�A$��A$�A"��A!�A ��A   A��A��A^5A�A�hA�DA{A��A"�AffA9XA$�A��A
=A�A��A�A��A�wA��AVAbNA|�A�9A{A��AZAAffA(�AG�A
=A�A	�mA�^A�7A��AQ�A �A�A�7A�A�jA�A ��@��F@��@��/@���@��+@��@��j@�ȴ@�Z@�ȴ@���@�j@�F@�ff@陚@�b@�G�@�A�@���@�M�@�@�K�@�-@ݩ�@�V@��@���@Չ7@�j@Ӿw@�V@���@϶F@��@�O�@��
@�=q@���@ȴ9@ȃ@�\)@�O�@�b@���@î@�+@�5?@���@��`@�r�@�\)@���@�J@�@�X@�V@��@���@�I�@���@��@�K�@���@��!@�@��@��@��@�@���@�n�@�-@��#@�O�@���@��D@�A�@�1@���@��!@�J@���@�O�@�p�@�7L@��j@�A�@��;@�ƨ@�K�@���@�v�@��@��h@�p�@�O�@���@��j@��@�Q�@�A�@�b@��;@�l�@�"�@�
=@���@�ȴ@�v�@�-@��T@���@��7@�G�@�7L@��@��@��@��w@�K�@���@�-@��T@�@��h@�V@���@�Z@��w@��w@�|�@�
=@�M�@��h@��/@���@�@��H@���@���@�M�@�@���@���@��h@�hs@��@��/@��j@��u@��@�j@�1'@��P@�;d@��+@�@�O�@��@���@���@��j@��@���@�Q�@��
@�S�@�o@��@��!@��+@��\@�M�@��T@�?}@�`B@�%@���@�Q�@�  @���@�\)@���@���@�~�@�v�@�ff@�E�@�{@���@�X@�Ĝ@���@��u@��D@��@�Q�@�1'@�9X@�1'@� �@�1@��w@��@�+@�ȴ@��@��R@���@��+@�n�@�V@�=q@�5?@�J@���@�G�@���@��9@��D@�bN@��m@��;@���@��w@��F@��F@���@�C�@���@��\@�n�@�^5@�M�@��@��@���@���@��h@�O�@�&�@�&�@��@�V@��/@��@�j@��
@�;d@��@��H@��!@��@�?}@��/@���@��P@{o@r�@ihs@a7L@[��@R��@I��@AG�@:�!@4Z@,�D@'+@!��@/@&�@��@n�@�@V@	G�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�ȴA���A���A���A���A��A�bNA���A�wA�DA�A�Q�A�JA�-A��A�Q�A�33A���A�S�A���A�
=A䝲A���A�VA���Aݛ�A��`A���Aٙ�A�;dAכ�A�dZA�&�A��#Aִ9A�1'AՋDAӲ-A��A�bNAѣ�AжFA�jAζFA͙�A�1A� �A��HA�%A���A��HA�$�AƉ7A���A��mA�hsA�"�A�A�bA���A���A���A��7A�A���A�=qA�K�A�dZA��HA�ZA�1A�%A��A�+A�A�9XA���A�Q�A�|�A�z�A�?}A��DA��A�&�A��mA��9A��A��jA��DA���A�33A�z�A�1A��mA���A���A��HA�dZA��DA��A��A��
A�A��A��jA��RA��
A�ƨA�ffA��
A��A�+A��RA��A��A�33A�5?A��
A�  A�r�A��A�-A��A��#A��A�ƨA��!A�A��!A�l�A�bNA�n�A|ZAw��Av  Arz�Ao/Amt�Ai&�Ae;dAcC�Aa�FA^ �A\�A[hsAY�^AX��AU��AT�HAT1AR�9AQ�AO��AN5?AM�AKC�AI�;AHffAG?}AE`BAB��AA33A>�A;��A:{A61'A41'A2�jA2bA1l�A/&�A.bA-;dA,9XA+�A*ffA(�A'�wA'VA&bNA%�A$��A$�A"��A!�A ��A   A��A��A^5A�A�hA�DA{A��A"�AffA9XA$�A��A
=A�A��A�A��A�wA��AVAbNA|�A�9A{A��AZAAffA(�AG�A
=A�A	�mA�^A�7A��AQ�A �A�A�7A�A�jA�A ��@��F@��@��/@���@��+@��@��j@�ȴ@�Z@�ȴ@���@�j@�F@�ff@陚@�b@�G�@�A�@���@�M�@�@�K�@�-@ݩ�@�V@��@���@Չ7@�j@Ӿw@�V@���@϶F@��@�O�@��
@�=q@���@ȴ9@ȃ@�\)@�O�@�b@���@î@�+@�5?@���@��`@�r�@�\)@���@�J@�@�X@�V@��@���@�I�@���@��@�K�@���@��!@�@��@��@��@�@���@�n�@�-@��#@�O�@���@��D@�A�@�1@���@��!@�J@���@�O�@�p�@�7L@��j@�A�@��;@�ƨ@�K�@���@�v�@��@��h@�p�@�O�@���@��j@��@�Q�@�A�@�b@��;@�l�@�"�@�
=@���@�ȴ@�v�@�-@��T@���@��7@�G�@�7L@��@��@��@��w@�K�@���@�-@��T@�@��h@�V@���@�Z@��w@��w@�|�@�
=@�M�@��h@��/@���@�@��H@���@���@�M�@�@���@���@��h@�hs@��@��/@��j@��u@��@�j@�1'@��P@�;d@��+@�@�O�@��@���@���@��j@��@���@�Q�@��
@�S�@�o@��@��!@��+@��\@�M�@��T@�?}@�`B@�%@���@�Q�@�  @���@�\)@���@���@�~�@�v�@�ff@�E�@�{@���@�X@�Ĝ@���@��u@��D@��@�Q�@�1'@�9X@�1'@� �@�1@��w@��@�+@�ȴ@��@��R@���@��+@�n�@�V@�=q@�5?@�J@���@�G�@���@��9@��D@�bN@��m@��;@���@��w@��F@��F@���@�C�@���@��\@�n�@�^5@�M�@��@��@���@���@��h@�O�@�&�@�&�@��@�V@��/@��@�j@��
@�;d@��@��H@��!@��@�?}@��/@���@��P@{o@r�@ihs@a7L@[��@R��@I��@AG�@:�!@4Z@,�D@'+@!��@/@&�@��@n�@�@V@	G�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
33B
33B
33B
33B
33B
1'B
/B
-B
+B
(�B
(�B
&�B
#�B
�B
�B
uB
+B
B
+B
1B
1B
B
B	��B	��B
B
hB
�B
,B
?}B
N�B
S�B
W
B
aHB
�{B
��B
�dB
�)B
�B
��B�BI�B��B��B��B�B7LBXBz�B�B�'B�B��B��B��B��B�B�B�XB�BhB;dBC�BI�BH�B@�B2-B$�B$�B"�B"�B!�B �B&�B/B+B%�B �B�BVBDBDBDB%BB�B�mB�)B��BɺBȴB�^B�FB�RBŢBȴBÖB�wB�qB�jB�FB�B��B�oB}�Bm�BI�B7LB0!B%�B�BDB��B�B�BB��B�XB��B��B�\Bt�Be`B^5B[#B[#BXBR�B:^BhB
�
B
�RB
r�B
1'B
DB	��B	�B	�B	ȴB	�B	�oB	�B	x�B	e`B	[#B	S�B	I�B	B�B	6FB	0!B	+B	$�B	�B	{B	PB	1B	  B��B�B�B�mB�5B�B��BŢB�qB�?B�B�B��B��B�!B�-B�!B�!B�B�B��B��B��B��B��B��B��B��B��B��B��B�uB�oB�\B�DB�1B�%B�B�B|�B|�B|�B{�B{�B|�B�1B�PB�\B�bB�hB��B��B�B�B�B�B��B��B��B��B��B��B��B�?B�9B��B��B�\B�\B�\B�VB�VB�\B�\B�bB�hB�\B�VB�PB�DB�1B�%B�B�B�B�B�B�hB��B��B��B��B��B��B��B�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�'B�-B�LB�wBĜBŢBƨBɺB��B��B�B�B�;B�NB�`B�fB�yB�B�B�B�B�B�B��B��B��B��B	B	B	%B	
=B	PB	\B	bB	oB	{B	�B	�B	�B	�B	�B	�B	 �B	$�B	'�B	+B	,B	-B	.B	1'B	33B	33B	6FB	9XB	<jB	@�B	A�B	B�B	D�B	E�B	G�B	G�B	I�B	K�B	M�B	P�B	R�B	S�B	T�B	XB	\)B	`BB	cTB	e`B	ffB	hsB	iyB	jB	k�B	o�B	u�B	v�B	y�B	~�B	� B	�B	�B	�B	�%B	�+B	�1B	�1B	�1B	�=B	�PB	�PB	�\B	�bB	�hB	�oB	�uB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�'B	�-B	�3B	�9B	�?B	�FB	�RB	�^B	�qB	�}B	��B	��B	B	ÖB	ŢB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�)B	�5B	�;B	�BB	�HB	�HB	�TB	�`B	�fB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
%B
+B
+B
+B
+B
1B
1B
1B
1B
1B
	7B
	7B
	7B
	7B

=B

=B

=B
DB
DB
JB
JB
JB

=B

=B

=B
DB
hB
�B
!�B
)�B
2-B
6FB
:^B
@�B
D�B
L�B
S�B
XB
^5B
dZB
k�B
p�B
s�B
u�B
x�B
z�B
~�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B
3=B
3:B
3<B
3=B
3<B
10B
/"B
-B
+B
(�B
(�B
&�B
#�B
�B
�B
~B
2B
B
4B
:B
;B
$B
B	��B	�B
&B
rB
�B
,B
?�B
N�B
S�B
WB
aJB
�B
��B
�iB
�,B
�B
��B�BI�B��B��B��B�B7MBXBz�B�B�%B�B��B��B��B��B�B�B�WB�BgB;eBC�BI�BH�B@�B21B$�B$�B"�B"�B!�B �B&�B/B+B%�B �B�BYBDBCBEB&BB�B�oB�(B��BɾBȵB�`B�IB�TBţBȳB×B�yB�qB�nB�FB�B��B�oB}�Bm�BI�B7KB0#B%�B�BBB��B�B�DB��B�UB��B��B�[Bt�Be`B^3B[#B[&BXBR�B:^BjB
�B
�UB
r�B
1+B
GB	�B	�B	�B	ȼB	�B	�yB	�"B	x�B	elB	[0B	TB	I�B	B�B	6SB	0.B	+B	$�B	�B	�B	^B	AB	 B��B��B�B�~B�HB�!B��BŴB��B�TB�.B�B�	B��B�4B�?B�5B�4B�.B� B�B��B��B��B��B��B��B��B��B��B��B��B��B�qB�WB�DB�:B�(B�B}B}B}B{�B{�B}B�HB�fB�rB�uB�}B��B��B�B�%B�(B�B�B�	B��B��B��B��B��B�RB�JB��B��B�pB�qB�qB�jB�hB�nB�oB�uB�|B�pB�kB�aB�WB�FB�:B�'B�B� B�(B�3B�|B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�-B�6B�@B�]B��BĮBųBƹB��B��B�B�&B�,B�LB�]B�mB�sB�B�B�B�B�B�B�B��B��B��B��B	B	B	2B	
IB	\B	hB	oB	|B	�B	�B	�B	�B	�B	�B	�B	 �B	$�B	'�B	+B	,B	-B	.!B	11B	3>B	3=B	6QB	9dB	<uB	@�B	A�B	B�B	D�B	E�B	G�B	G�B	I�B	K�B	M�B	P�B	R�B	TB	UB	XB	\4B	`KB	c_B	eiB	fpB	hB	i�B	j�B	k�B	o�B	u�B	v�B	y�B	B	�B	�B	�B	�B	�.B	�4B	�9B	�9B	�<B	�GB	�XB	�YB	�cB	�lB	�oB	�uB	�~B	�~B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�/B	�4B	�=B	�AB	�GB	�LB	�WB	�dB	�wB	��B	��B	��B	B	ÝB	ũB	ȺB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�"B	�/B	�<B	�@B	�JB	�MB	�NB	�ZB	�fB	�lB	�zB	�~B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 B
B
B
)B
2B
2B
0B
1B
7B
4B
8B
6B
7B
	=B
	;B
	;B
	;B

BB

AB

BB
JB
LB
PB
NB
PB

AB

AB

CB
JB
nB
�B
!�B
)�B
21B
6JB
:`B
@�B
D�B
L�B
S�B
XB
^7B
d[B
k�B
p�B
s�B
u�B
x�B
z�B
~�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.01 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311214452016053112144520160531121445  AO  ARCAADJP                                                                    20150913191614    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150913191614  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20150913191614  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531121445  IP                  G�O�G�O�G�O�                