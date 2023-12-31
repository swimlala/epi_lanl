CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-07-03T19:16:30Z AOML 3.0 creation; 2016-05-31T19:14:44Z UW 3.1 conversion     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20150703191630  20160531121444  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               uA   AO  4051_7090_117                   2C  D   APEX                            5368                            041511                          846 @�]�m��1   @�]"��@4
=p���dO|�hs1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    uA   A   A   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BXffB`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C�C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� DrfDr� Ds  Ds� Dt  Dty�Dy` D��D�L�D�s3D�ɚD���D�33D�s3D��fD�fD�I�D���D��fD�  D�0 D�vfD��D�fD�VfD� D�ɚ11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��R@�Q�A (�A (�A@(�A`(�A�{A�{A�{A�{A�{A�{A�{A�{B 
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
=BXp�B`
=Bh
=Bp
=Bx
=B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�C �C�C�C�C�C
�C�C�C�C�C�C�C�C�C�C)C �C"�C$�C&�C(�C*�C,�C.�C0�C2�C4�C6�C8�C:�C<�C>�C@�CB�CD�CF�CH�CJ�CL�CN�CP�CR�CT�CV�CX�CZ�C\�C^�C`�Cb�Cd�Cf�Ch�Cj�Cl�Cn�Cp�Cr�Ct�Cv�Cx�Cz�C|�C~�C�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�C�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�C�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HD  �D ��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D	 �D	��D
 �D
��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D  �D ��D! �D!��D" �D"��D# �D#��D$ �D$��D% �D%��D& �D&��D' �D'��D( �D(��D) �D)��D* �D*��D+ �D+��D, �D,��D- �D-��D. �D.��D/ �D/��D0 �D0��D1 �D1��D2 �D2��D3 �D3��D4 �D4��D5 �D5��D6 �D6��D7 �D7��D8 �D8��D9 �D9��D: �D:��D; �D;��D< �D<��D= �D=��D> �D>��D? �D?��D@ �D@��DA �DA��DB �DB��DC �DC��DD �DD��DE �DE��DF �DF��DG �DG��DH �DH��DI �DI��DJ �DJ��DK �DK��DL �DL��DM �DM��DN �DN��DO �DO��DP �DP��DQ �DQ��DR �DR��DS �DS��DT �DT��DU �DU��DV �DV��DW �DW��DX �DX��DY �DY��DZ �DZ��D[ �D[��D\ �D\��D] �D]��D^ �D^��D_ �D_��D` �D`��Da �Da��Db �Db��Dc �Dc��Dd �Dd��De �De��Df �Df��Dg �Dg��Dh �Dh��Di �Di��Dj �Dj��Dk �Dk��Dl �Dl��Dm �Dm��Dn �Dn��Do �Do��Dp �Dp��Dq �Dq��Dr
Dr��Ds �Ds��Dt �Dtz>Dy`�D�D�MD�s�D���D��D�3�D�s�D�ƸD��D�I�D��D�ƸD� RD�0RD�v�D�D��D�V�D�RD���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A���Aۛ�A�^5Aډ7A�(�A���A��;A���A�ȴAٙ�A�n�A�VA�?}A���Aأ�A�|�AؑhA�|�A�O�A�/A׍PA�C�A�jA�/A��A���A�G�AϸRA�A�`BA��mA�7LA��A��/A��Aɏ\A�/AȮA�A�{Aƛ�A�l�A��mA�n�A´9A�K�A�JA��wA�C�A���A�(�A�VA�A�C�A��yA��A���A��
A���A��A��7A��yA�=qA���A���A���A��7A� �A�ZA�-A�v�A���A�bA�bNA�r�A�ȴA�A�jA�jA��A�"�A��/A���A�-A�dZA��PA���A��!A�ƨA���A�
=A��RA�?}A�S�A�S�A���A��A��mA���A�^5A�"�A�?}A�~�A���A�1A��yA��!A���A���A�I�A��A��DA���A���A�?}A��A}"�A|=qA{Ayt�Ax�Ax=qAvr�At�9Ar�Ap~�An�uAm��Al�yAl(�AkdZAj��Ah�RAh�Af�RAe��Ac|�A^�A[�-AZ�AX�AW�#AT�\AQoAMS�AIS�AHffAG��AG�AFA�AD��ACS�AA��A@JA?�A?XA>�A=�A:�`A8$�A6^5A6 �A5�
A5�A4�uA1�A0�+A/��A/l�A.�!A-x�A,�A+hsA*I�A)VA((�A&�!A$ȴA"ĜA �AA�AXA�AA�Ap�AZAJA�`A�+AA�A�-A�A��A{A��AƨA
=A�/A�A5?A
��A	%A�AI�A��AC�AJA��A�A��AffA�mA��A?}AAr�A�#A �j@�n�@�  @�^5@��T@��7@�p�@��@�9X@�\)@� �@�;d@�t�@�{@�&�@��@�&�@㕁@ᙚ@��;@�33@�ff@�z�@۝�@�~�@��@�E�@���@�(�@�
=@с@мj@�\)@�5?@�9X@���@˝�@�t�@��@��@�r�@�33@Ų-@þw@��@�z�@�ƨ@�5?@�?}@��@�bN@�  @�r�@���@���@�X@��@��@�p�@�/@��j@��@�+@�-@��^@�/@�I�@�(�@�b@��m@���@���@�=q@���@���@��@�%@��j@�r�@�1'@��w@��@��y@�5?@���@�?}@�%@��/@��u@�I�@�1@���@�l�@�S�@�C�@�o@��R@��\@�n�@�@�/@��u@�A�@��@���@�|�@�dZ@�\)@�33@�~�@�p�@�/@��`@���@�hs@��@��-@�Ĝ@�b@�z�@��@��@�z�@��@��w@���@�
=@���@�~�@��@�`B@��@��9@�I�@��F@�C�@�
=@���@�M�@�-@�-@��@���@��^@��7@�&�@���@�j@�1@��@�S�@��R@�V@�J@��@�@�hs@��@��@���@��u@�Z@�1'@�b@���@���@�l�@�K�@�;d@�33@�+@�@��y@���@���@�^5@�{@�@��T@��^@���@���@��h@��@�x�@�hs@�?}@�%@�Ĝ@�r�@�bN@�Q�@�I�@�A�@�(�@�ƨ@���@��@�+@��!@���@���@�$�@��T@���@���@�p�@�7L@���@�bN@���@�33@���@��R@��\@���@���@�&�@�%@�%@�Ĝ@��@�I�@��@��@�\)@�S�@�K�@�;d@���@���@��H@���@�~�@�{@��@���@���@�@���@�X@��/@��9@��@��D@�I�@� �@��w@��@�;d@��@���@�V@�-@��@��@�@���@��@�O�@�7L@��@�j@�Z@�1@��;@���@�S�@�@���@��@+@qG�@j�@c�@_
=@Y%@PQ�@I��@A��@;�
@7;d@2-@,Z@(  @"�@�@�P@-@p�@	&�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A���Aۛ�A�^5Aډ7A�(�A���A��;A���A�ȴAٙ�A�n�A�VA�?}A���Aأ�A�|�AؑhA�|�A�O�A�/A׍PA�C�A�jA�/A��A���A�G�AϸRA�A�`BA��mA�7LA��A��/A��Aɏ\A�/AȮA�A�{Aƛ�A�l�A��mA�n�A´9A�K�A�JA��wA�C�A���A�(�A�VA�A�C�A��yA��A���A��
A���A��A��7A��yA�=qA���A���A���A��7A� �A�ZA�-A�v�A���A�bA�bNA�r�A�ȴA�A�jA�jA��A�"�A��/A���A�-A�dZA��PA���A��!A�ƨA���A�
=A��RA�?}A�S�A�S�A���A��A��mA���A�^5A�"�A�?}A�~�A���A�1A��yA��!A���A���A�I�A��A��DA���A���A�?}A��A}"�A|=qA{Ayt�Ax�Ax=qAvr�At�9Ar�Ap~�An�uAm��Al�yAl(�AkdZAj��Ah�RAh�Af�RAe��Ac|�A^�A[�-AZ�AX�AW�#AT�\AQoAMS�AIS�AHffAG��AG�AFA�AD��ACS�AA��A@JA?�A?XA>�A=�A:�`A8$�A6^5A6 �A5�
A5�A4�uA1�A0�+A/��A/l�A.�!A-x�A,�A+hsA*I�A)VA((�A&�!A$ȴA"ĜA �AA�AXA�AA�Ap�AZAJA�`A�+AA�A�-A�A��A{A��AƨA
=A�/A�A5?A
��A	%A�AI�A��AC�AJA��A�A��AffA�mA��A?}AAr�A�#A �j@�n�@�  @�^5@��T@��7@�p�@��@�9X@�\)@� �@�;d@�t�@�{@�&�@��@�&�@㕁@ᙚ@��;@�33@�ff@�z�@۝�@�~�@��@�E�@���@�(�@�
=@с@мj@�\)@�5?@�9X@���@˝�@�t�@��@��@�r�@�33@Ų-@þw@��@�z�@�ƨ@�5?@�?}@��@�bN@�  @�r�@���@���@�X@��@��@�p�@�/@��j@��@�+@�-@��^@�/@�I�@�(�@�b@��m@���@���@�=q@���@���@��@�%@��j@�r�@�1'@��w@��@��y@�5?@���@�?}@�%@��/@��u@�I�@�1@���@�l�@�S�@�C�@�o@��R@��\@�n�@�@�/@��u@�A�@��@���@�|�@�dZ@�\)@�33@�~�@�p�@�/@��`@���@�hs@��@��-@�Ĝ@�b@�z�@��@��@�z�@��@��w@���@�
=@���@�~�@��@�`B@��@��9@�I�@��F@�C�@�
=@���@�M�@�-@�-@��@���@��^@��7@�&�@���@�j@�1@��@�S�@��R@�V@�J@��@�@�hs@��@��@���@��u@�Z@�1'@�b@���@���@�l�@�K�@�;d@�33@�+@�@��y@���@���@�^5@�{@�@��T@��^@���@���@��h@��@�x�@�hs@�?}@�%@�Ĝ@�r�@�bN@�Q�@�I�@�A�@�(�@�ƨ@���@��@�+@��!@���@���@�$�@��T@���@���@�p�@�7L@���@�bN@���@�33@���@��R@��\@���@���@�&�@�%@�%@�Ĝ@��@�I�@��@��@�\)@�S�@�K�@�;d@���@���@��H@���@�~�@�{@��@���@���@�@���@�X@��/@��9@��@��D@�I�@� �@��w@��@�;d@��@���@�V@�-@��@��@�@���@��@�O�@�7L@��@�j@�Z@�1@��;@���@�S�@�@���@��@+@qG�@j�@c�@_
=@Y%@PQ�@I��@A��@;�
@7;d@2-@,Z@(  @"�@�@�P@-@p�@	&�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
aHB
33B
/B
%�B
 �B
�B
 �B
!�B
#�B
#�B
%�B
&�B
'�B
%�B
,B
=qB
G�B
S�B
aHB
gmB
p�B
r�B
~�B
��B
��B
��BVB�B)�B<jBYBo�B�\B�-B�/B�B:^BF�B_;BffBz�B�uB�uB��B�-B�LB�FB�RB�HB��BB��B��B�B�TB�B�`B��BB�B�
B�B�B��B��B��B��BffB%�BhB\BVBB��B��BB��B�B��B��B��B�9B��B!�B49B+BN�BP�BhsB\)BN�B49B{B��B�}B��B�?B��B��Bz�BffBVB:^B,B�BJB
��B
�;B
ÖB
�B
��B
��B
�uB
�PB
�B
^5B
E�B
=qB
33B
)�B
$�B
�B
hB
  B	�fB	��B	ÖB	�}B	�qB	�qB	�dB	�LB	�B	�RB	�9B	�B	��B	� B	e`B	\)B	S�B	L�B	;dB	33B	 �B	�B	{B	hB	\B	DB	%B	B��B��B�B�B�B�fB�BB�
B��B��B��B��BƨB�wB�^B�RB�FB�3B�!B�B�B��B��B��B��B��B�uB�bB�bB�\B�\B�\B�VB�PB�\B�\B�VB�VB�bB�hB�1B�Bw�Bv�Br�Bo�Bn�Bl�BjBgmBe`BffBe`Be`BffBe`BdZBdZBe`BiyBgmBffBe`Bk�Bo�Bl�BffB`BB[#B\)B_;BbNBcTBhsBo�Br�B� B�PB}�BhsBS�BM�BD�BA�BB�B@�B@�B@�BB�BB�BA�B@�B?}B>wB>wB@�BG�BL�BR�BT�B\)B]/B]/B\)B\)B]/B^5BaHBe`BgmBiyBm�Bo�Bw�B{�B}�B� B�%B��B��B��B�dB��B��B��B��B�B�#B�5B�HB�TB�fB�B��B��B��B��B��B	B	B		7B	JB	\B	bB	bB	hB	uB	�B	�B	�B	 �B	$�B	&�B	'�B	(�B	+B	,B	.B	/B	/B	0!B	2-B	5?B	6FB	7LB	9XB	=qB	@�B	A�B	C�B	D�B	E�B	E�B	E�B	E�B	E�B	J�B	N�B	O�B	Q�B	ZB	_;B	bNB	bNB	bNB	ffB	iyB	k�B	n�B	q�B	s�B	t�B	x�B	{�B	{�B	�B	�+B	�7B	�=B	�DB	�\B	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�'B	�'B	�3B	�9B	�XB	�^B	�^B	�^B	�dB	�jB	�wB	�wB	�}B	��B	ÖB	ŢB	ǮB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�)B	�/B	�/B	�5B	�BB	�BB	�HB	�NB	�TB	�TB	�ZB	�`B	�mB	�sB	�mB	�fB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
B
B
B
B
%B
+B
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

=B

=B
DB
DB
DB
JB
PB
PB
PB
�B
�B
&�B
/B
49B
8RB
?}B
E�B
J�B
P�B
T�B
ZB
_;B
cTB
hsB
l�B
p�B
u�B
y�B
}�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B
aNB
3>B
/!B
%�B
 �B
�B
 �B
!�B
#�B
#�B
%�B
&�B
'�B
%�B
,B
=vB
G�B
S�B
aLB
grB
p�B
r�B
~�B
��B
��B
��BXB�B)�B<jBYBo�B�\B�,B�.B�B:ZBF�B_?BfcBz�B�uB�uB��B�-B�JB�DB�RB�GB��B!B��B��B�B�SB�B�aB��BB�B�	B�B�B��B��B��B��BffB%�BfBYBQBB��B��BB��B�B��B��B��B�9B��B!�B49B+ BN�BP�BhqB\,BN�B4:BB��B�{B��B�?B��B��Bz�BfdBVB:\B,	B�BNB
��B
�=B
ÚB
�B
��B
��B
�wB
�RB
�B
^:B
E�B
=vB
38B
* B
$�B
�B
oB
 B	�oB	��B	ÞB	��B	�zB	�{B	�nB	�SB	�#B	�]B	�CB	�B	��B	�	B	elB	\5B	TB	L�B	;sB	3BB	 �B	�B	�B	vB	kB	SB	3B	$B��B��B��B�B�B�yB�TB�B��B��B��B��BƺB��B�qB�dB�YB�EB�6B�"B�B��B��B��B��B��B��B�xB�uB�oB�pB�sB�kB�fB�tB�pB�iB�hB�vB�}B�FB�Bw�Bv�Br�Bo�Bn�Bl�Bj�Bg�BexBf{BewBeuBf|BeuBdmBdoBevBi�Bg�BfzBeuBk�Bo�Bl�Bf~B`XB[=B\>B_OBbcBckBh�Bo�Br�B�B�bB~Bh�BTBM�BD�BA�BB�B@�B@�B@�BB�BB�BA�B@�B?�B>�B>�B@�BG�BL�BSBUB\<B]AB]BB\>B\>B]DB^JBa^BetBg�Bi�Bm�Bo�Bw�B{�B~B�B�6B��B��B��B�qB��B��B�B�B�B�/B�DB�WB�fB�uB�B��B��B��B��B��B	B	'B		DB	WB	hB	oB	rB	vB	�B	�B	�B	�B	 �B	$�B	&�B	'�B	) B	+B	,B	. B	/(B	/$B	0*B	29B	5JB	6QB	7WB	9cB	=zB	@�B	A�B	C�B	D�B	E�B	E�B	E�B	E�B	E�B	J�B	N�B	O�B	Q�B	Z(B	_GB	bYB	bYB	bVB	foB	i�B	k�B	n�B	q�B	s�B	t�B	x�B	{�B	{�B	�B	�2B	�?B	�EB	�JB	�cB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�"B	�%B	�#B	�#B	�B	�B	�B	�B	�#B	�0B	�.B	�<B	�?B	�^B	�dB	�eB	�gB	�jB	�rB	��B	�~B	��B	��B	ÝB	ŧB	ǶB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�
B	�B	�B	�B	�B	�&B	�.B	�6B	�3B	�;B	�JB	�KB	�OB	�UB	�[B	�YB	�`B	�cB	�sB	�xB	�rB	�lB	�xB	�zB	�~B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	� B

B
B
B

B
B
B
B
B
B
B
$B
"B
*B
5B
2B
3B
2B
3B
1B
8B
8B
6B
6B
6B
6B
6B

BB

BB
JB
KB
IB
MB
UB
UB
SB
�B
�B
&�B
/ B
4?B
8UB
?�B
E�B
J�B
P�B
UB
ZB
_>B
cXB
hvB
l�B
p�B
u�B
y�B
}�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.01 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311214442016053112144420160531121444  AO  ARCAADJP                                                                    20150703191630    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150703191630  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20150703191630  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531121444  IP                  G�O�G�O�G�O�                