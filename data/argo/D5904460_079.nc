CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-10-15T19:21:22Z AOML 3.0 creation; 2016-08-07T21:17:41Z UW 3.1 conversion     
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
_FillValue                 �  Ax   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  Cp   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  KH   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M@   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  U   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ^�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  f�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  h�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  p�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  xh   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  z`   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �8   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �0   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �8   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �8   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �8   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �8   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �d   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �h   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �l   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �p   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �t   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20151015192122  20160807141741  5904460 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               OA   AO  5285_8895_079                   2C  D   APEX                            6487                            072314                          846 @�w,;Z��1   @�w,���@+;�l�C��c̛��S�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    OA   B   B   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A���A���Aݙ�A���A�33B��B  B  B   B(  B0  B8ffB@ffBH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  B�  B�  B���B�  B�  B�  B�  B���B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DDy�DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DMy�DM��DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]fD]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Doy�Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt` DyS3D�	�D�L�D��3D���D�3D�I�D�y�D��fD��fD�L�D���Dǰ D�Ff1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�G�@�{A
=A'
=AG
=Ag
=A��A��A��A��A�Q�A�Q�A��A�Q�B\)B	\)BBB!B)B1B:(�BB(�BIBQBYBaBiBqByB��HB��HB��HB��HB��HB��HB��HB��HB��HB��B��HB��HB��HB��HB��HB��HB��HB��HB�{B�{B��HB��HB��HBܮB��HB��HB��HB��HB�B��HB��HB��HC p�Cp�Cp�Cp�Cp�C
p�Cp�Cp�Cp�Cp�Cp�Cp�Cp�Cp�Cp�Cp�C p�C"p�C$p�C&p�C(p�C*p�C,p�C.p�C0p�C2p�C4p�C6p�C8p�C:p�C<p�C>p�C@p�CBp�CDp�CFp�CHp�CJp�CLp�CNp�CPp�CRp�CTp�CVp�CXp�CZp�C\p�C^p�C`p�Cbp�Cdp�Cfp�Chp�Cjp�Clp�Cnp�Cpp�Crp�Ctp�Cvp�Cxp�Czp�C|p�C~p�C�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RD )D �)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D	)D	�)D
)D
�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D )D �)D!)D!�)D")D"�)D#)D#�)D$)D$�)D%)D%�)D&)D&�)D')D'�)D()D(�)D))D)�)D*)D*�)D+)D+�)D,)D,�)D-)D-�)D.)D.�)D/)D/�)D0)D0�)D1)D1�)D2)D2�)D3)D3�)D4)D4�)D5)D5�)D6)D6�)D7)D7�)D8)D8�)D9)D9�)D:)D:�)D;)D;�)D<)D<�)D=)D=�)D>)D>�)D?)D?�)D@)D@�)DA)DA�)DB)DB�)DC)DC�)DD)DD��DE)DE�)DF)DF�)DG)DG�)DH)DH�)DI)DI�)DJ)DJ�)DK)DK�)DL)DL�)DM)DM��DN�DN�)DO)DO�)DP)DP�)DQ)DQ�)DR)DR�)DS)DS�)DT)DT�)DU)DU�)DV)DV�)DW)DW�)DX)DX�)DY)DY�)DZ)DZ�)D[)D[�)D\)D\�)D]"�D]�)D^)D^�)D_)D_�)D`)D`�)Da)Da�)Db)Db�)Dc)Dc�)Dd)Dd�)De)De�)Df)Df�)Dg)Dg�)Dh)Dh�)Di)Di�)Dj)Dj�)Dk)Dk�)Dl)Dl�)Dm)Dm�)Dn)Dn�)Do)Do��Dp)Dp�)Dq)Dq�)Dr)Dr�)Ds)Ds�)Dt)Dt|)Dyo\D��D�Z�D��GD���D�!GD�W�D���D��zD�zD�Z�D���DǾD�Tz1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A���A���A���A���A���A���A���A���A���A���A���A���A�\)A��HA�/A�33A��yA��`A�\)A�r�A� �A�z�A��#A�$�AǁA�S�AÓuA��A��A��DA�K�A��7A�VA�{A�ƨA�v�A�?}A�r�A��hA��DA��A�C�A�r�A��A���A�A�;dA���A��A�$�A?}AzffAt1AlĜAe&�A`5?AZȴAXbNATn�ARQ�AP��AN�yAL�yAL-AK�AE��AC7LAB�AA�A@Q�A?��A=A9�PA5�-A4JA3\)A1�PA0��A01'A/|�A/��A-��A+��A+x�A*�yA%?}A!�#A!\)A!�#A!&�AƨA��A5?A|�A��AĜA��A�AQ�A=qA$�A��AA�A-A�A�mA�^A��A�AG�AK�A��A�A��A��A��A=qA(�A�A?}AjA{A;dAVAZAK�A^5At�A�/A�uAn�A|�A%A�jA�jAA�A`BA%A�9AffAA7LA
�yA	K�AbNAVAAƨA��AVA�\A1'A�wA��AK�A�DAM�A1'AbA��A�A�Ar�AbNAA�A9XA{A|�A�A �HA ��A �!A   @�v�@���@�z�@�;d@�o@��y@�ff@��h@���@��w@��@���@�|�@�o@�7L@�b@�@�\)@�@�h@�1'@@�ȴ@�p�@�  @�;d@�V@�+@�n�@��@�j@�C�@�^5@�X@�V@�j@�(�@�;d@��@�j@�Z@ߥ�@ާ�@ݩ�@��@���@ܛ�@�bN@�b@۝�@�n�@��#@ٺ^@�X@���@�A�@���@�t�@��@֏\@���@ԃ@��;@ӕ�@Ұ!@�J@��@�j@�dZ@��y@�n�@�J@ͺ^@��`@�Z@�A�@�1@�dZ@���@�$�@�@�hs@��/@�Z@� �@�  @��m@ǶF@�o@�^5@�7L@��`@���@ă@�bN@�(�@å�@�33@��@\@�V@�5?@�$�@�{@�@��T@��@�Ĝ@�9X@���@��@��+@���@��#@��h@�G�@�&�@��`@��j@���@�j@�  @��@��@��@�|�@�K�@���@�v�@�$�@���@��^@�x�@��@��@���@��u@�bN@�1'@���@��
@��P@�S�@�33@��y@�n�@���@���@�9X@��m@�;d@���@�n�@�{@��T@��7@���@�1@��P@�t�@�\)@�@�@�7L@��j@�bN@�9X@�1@�\)@�+@�;d@��@�M�@��@��h@��@�z�@��@��w@�t�@�C�@��H@�n�@���@��/@���@�bN@�1'@� �@�  @��
@���@�l�@�33@��y@�~�@�V@�=q@�-@�-@�{@���@�X@���@��j@���@��u@��@�r�@�9X@��F@�t�@�
=@���@�n�@�=q@��@�@��7@�G�@���@��@�Q�@� �@��;@��@�|�@�+@��!@��+@�V@�$�@��#@��^@��-@�G�@��@��@�Q�@��@���@�l�@��R@�n�@��@�p�@�X@��@�Ĝ@���@��@�Q�@�(�@��@��@��!@��+@�V@��@��h@�V@���@�I�@�b@�  @��@�|�@��@�v�@�ff@�ff@�M�@�$�@�@��^@��7@�x�@�hs@�`B@�7L@��@���@���@�r�@�b@���@�ƨ@��F@��P@�K�@���@��+@�n�@�5?@�p�@�7L@��@��@�Ĝ@��@��@�z�@��m@���@�ƨ@��F@���@��P@�S�@�+@�o@���@���@���@��+@�b@}O�@r��@j��@b-@Z=q@Tz�@N��@D��@>E�@5/@-��@'�@ r�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111  A���A���A���A���A���A���A���A���A���A���A���A���A�\)A��HA�/A�33A��yA��`A�\)A�r�A� �A�z�A��#A�$�AǁA�S�AÓuA��A��A��DA�K�A��7A�VA�{A�ƨA�v�A�?}A�r�A��hA��DA��A�C�A�r�A��A���A�A�;dA���A��A�$�A?}AzffAt1AlĜAe&�A`5?AZȴAXbNATn�ARQ�AP��AN�yAL�yAL-AK�AE��AC7LAB�AA�A@Q�A?��A=A9�PA5�-A4JA3\)A1�PA0��A01'A/|�A/��A-��A+��A+x�A*�yA%?}A!�#A!\)A!�#A!&�AƨA��A5?A|�A��AĜA��A�AQ�A=qA$�A��AA�A-A�A�mA�^A��A�AG�AK�A��A�A��A��A��A=qA(�A�A?}AjA{A;dAVAZAK�A^5At�A�/A�uAn�A|�A%A�jA�jAA�A`BA%A�9AffAA7LA
�yA	K�AbNAVAAƨA��AVA�\A1'A�wA��AK�A�DAM�A1'AbA��A�A�Ar�AbNAA�A9XA{A|�A�A �HA ��A �!A   @�v�@���@�z�@�;d@�o@��y@�ff@��h@���@��w@��@���@�|�@�o@�7L@�b@�@�\)@�@�h@�1'@@�ȴ@�p�@�  @�;d@�V@�+@�n�@��@�j@�C�@�^5@�X@�V@�j@�(�@�;d@��@�j@�Z@ߥ�@ާ�@ݩ�@��@���@ܛ�@�bN@�b@۝�@�n�@��#@ٺ^@�X@���@�A�@���@�t�@��@֏\@���@ԃ@��;@ӕ�@Ұ!@�J@��@�j@�dZ@��y@�n�@�J@ͺ^@��`@�Z@�A�@�1@�dZ@���@�$�@�@�hs@��/@�Z@� �@�  @��m@ǶF@�o@�^5@�7L@��`@���@ă@�bN@�(�@å�@�33@��@\@�V@�5?@�$�@�{@�@��T@��@�Ĝ@�9X@���@��@��+@���@��#@��h@�G�@�&�@��`@��j@���@�j@�  @��@��@��@�|�@�K�@���@�v�@�$�@���@��^@�x�@��@��@���@��u@�bN@�1'@���@��
@��P@�S�@�33@��y@�n�@���@���@�9X@��m@�;d@���@�n�@�{@��T@��7@���@�1@��P@�t�@�\)@�@�@�7L@��j@�bN@�9X@�1@�\)@�+@�;d@��@�M�@��@��h@��@�z�@��@��w@�t�@�C�@��H@�n�@���@��/@���@�bN@�1'@� �@�  @��
@���@�l�@�33@��y@�~�@�V@�=q@�-@�-@�{@���@�X@���@��j@���@��u@��@�r�@�9X@��F@�t�@�
=@���@�n�@�=q@��@�@��7@�G�@���@��@�Q�@� �@��;@��@�|�@�+@��!@��+@�V@�$�@��#@��^@��-@�G�@��@��@�Q�@��@���@�l�@��R@�n�@��@�p�@�X@��@�Ĝ@���@��@�Q�@�(�@��@��@��!@��+@�V@��@��h@�V@���@�I�@�b@�  @��@�|�@��@�v�@�ff@�ff@�M�@�$�@�@��^@��7@�x�@�hs@�`B@�7L@��@���@���@�r�@�b@���@�ƨ@��F@��P@�K�@���@��+@�n�@�5?@�p�@�7L@��@��@�Ĝ@��@��@�z�@��m@���@�ƨ@��F@���@��P@�S�@�+@�o@���@���@���G�O�@�b@}O�@r��@j��@b-@Z=q@Tz�@N��@D��@>E�@5/@-��@'�@ r�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;oB	B	B	B	B	B	B	B	B	B	B	B	VB	$�B
+B
iyB
s�B
�B
x�B
~�B
�B
�uB
��B
��B
�!B
�ZBbB$�BJ�BM�BP�Bs�Br�B]/BW
BP�BH�B�B
��B	��B	��B	�JB	y�B	t�B	t�B	s�B	v�B	v�B	}�B	�JB	�B	v�B	�7B	�B	YB	)�B	bB��B�B�mB�B�mB�NB�B	DB	\B��B�mB�HB�#B�B��B�)B��B�wB�LB�3B�9B�?B�9BŢB�fB��B��B��B�B��B�XB�LB��B�TB�5B�5B�TB�HB�yB��B	�B	#�B	,B	1'B	2-B	33B	<jB	G�B	L�B	Q�B	T�B	^5B	q�B	�B	�7B	��B	��B	��B	��B	�B	�3B	�qB	ÖB	ÖB	�}B	��B	�qB	��B	�qB	�3B	�B	��B	�3B	�3B	�LB	�?B	�9B	�?B	�jB	�wB	�jB	�jB	�dB	�^B	�XB	�dB	�wB	�RB	�RB	�XB	�dB	�jB	�jB	�qB	�jB	�qB	�}B	�wB	�}B	��B	��B	�}B	�wB	�wB	�qB	�qB	ĜB	��B	��B	��B	��B	��B	��B	��B	�
B	�#B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�
B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	ǮB	ȴB	ƨB	ĜB	ŢB	ÖB	��B	��B	��B	��B	ɺB	ǮB	ĜB	ǮB	��B	��B	��B	��B	�B	�B	�B	�
B	�
B	�
B	�
B	�
B	�
B	�
B	�B	�B	�B	�#B	�)B	�)B	�#B	�#B	�#B	�;B	�HB	�HB	�HB	�NB	�TB	�TB	�TB	�TB	�NB	�NB	�NB	�NB	�NB	�NB	�TB	�ZB	�`B	�`B	�`B	�fB	�mB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
%B
+B
1B
	7B

=B
	7B
	7B

=B
DB
JB
PB
PB
PB
PB
\B
\B
\B
bB
bB
bB
bB
bB
bB
bB
hB
hB
oB
oB
oB
oB
oB
hB
oB
oB
oB
uB
uB
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
 �B
!�B
 �B
 �B
 �B
 �B
!�B
"�B
#�B
#�B
$�B
%�B
%�B
%�B
%�B
%�B
%�B
&�B
&�B
&�B
&�B
&�B
'�B
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
)�B
(�B
)�B
)�B
)�B
)�B
)�B
)�B
)�B
)�B
+B
+B
+B
+B
+B
+B
+B
+B
+B
+B
+B
+B
.B
,B
33B
<jB
A�B
F�B
L�B
Q�B
T�B
[#B
^5B
cTB
iyB
m�B
q�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111  B	B	B	B	B	B	B	B	B	B	B	B	HB	$�B
*�B
i\B
s�B
��B
x�B
~�B
��B
�YB
��B
��B
�B
�;B>B$�BJ�BM�BP�Bs�Br�B]BV�BP�BH�BtB
��B	��B	��B	�,B	y�B	t�B	t�B	s�B	v�B	v�B	}�B	�*B	��B	v�B	�B	��B	X�B	)�B	GB��B�nB�OB�aB�PB�2B�sB	!B	<B��B�NB�*B�B��B��B�B��B�XB�,B�B�B� B�BŁB�DB��B��B��B�}BͲB�7B�*BϽB�2B�B�B�2B�%B�WB��B	{B	#�B	+�B	0�B	2B	3B	<AB	G�B	L�B	Q�B	T�B	^B	q�B	��B	�B	�VB	��B	��B	��B	��B	�
B	�DB	�hB	�iB	�QB	�XB	�FB	�^B	�GB	�B	��B	��B	�B	�B	�!B	�B	�B	�B	�@B	�KB	�=B	�=B	�:B	�2B	�*B	�6B	�KB	�$B	�%B	�+B	�:B	�<B	�=B	�CB	�<B	�EB	�NB	�MB	�MB	�VB	�VB	�OB	�KB	�KB	�DB	�CB	�oB	˘B	̟B	ͣB	̠B	ͥB	ͦB	ϰB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	ѽB	ʓB	ǁB	ȇB	�zB	�mB	�sB	�gB	˘B	ΩB	ΫB	̜B	ɎB	ǀB	�lB	ǂB	̞B	ͣB	ѿB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�	B	�B	�B	�B	�B	�$B	�$B	�!B	�$B	�B	�B	�B	�B	�B	�B	�"B	�)B	�0B	�1B	�0B	�6B	�<B	�CB	�CB	�IB	�KB	�VB	�UB	�SB	�TB	�XB	�ZB	�`B	�^B	�\B	�gB	�eB	�fB	�fB	�rB	�tB	�uB	�tB	�qB	�rB	�rB	�rB	�nB	�jB	�aB	�\B	�RB	�SB	�UB	�[B	�]B	�XB	�\B	�\B	�bB	�aB	�`B	�^B	�eB	�lB	�lB	�mB	�gB	�hB	�eB	�mB	�rB	�rB	�sB	�qB	�{B	�vB	�|B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
	B

B
	B
	B

	B
B
B
B
B
B
B
(B
+B
+B
.B
/B
1B
/B
-B
.B
1B
5B
3B
<B
<B
:B
:B
:B
4B
9B
9B
:B
AB
CB
IB
FB
NB
SB
PB
ZB
XB
aB
cB
_B
_B
_B
dB
kB
lB
lB
sB
tB
tB
sB
rB
vB
xB
B
�B
}B
}B
zB
wB
xB
yB
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
 �B
 �B
 �B
 �B
!�B
"�B
#�B
#�B
$�B
%�B
%�B
%�B
%�B
%�B
%�B
&�B
&�B
&�B
&�B
&�B
'�B
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
)�B
(�B
)�B
)�B
)�B
)�B
)�B
)�B
)�B
)�B
*�B
*�B
*�B
*�B
*�B
*�B
*�B
*�B
*�B
*�B
*�B
*�G�O�B
+�B
3B
<6B
ATB
FsB
L�B
Q�B
T�B
Z�B
]�B
cB
iCB
m[B
qs1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.44 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071417412016080714174120160807141741  AO  ARCAADJP                                                                    20151015192122    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20151015192122  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20151015192122  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807141741  IP                  G�O�G�O�G�O�                