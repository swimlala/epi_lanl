CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2014-09-19T22:03:44Z AOML 3.0 creation; 2016-05-31T19:14:39Z UW 3.1 conversion     
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
resolution        :�o     �  U�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ]�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  _�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    gx   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ix   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  qp   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    yh   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {h   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �`   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �`   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �X   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �(   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �,   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �0   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �4Argo profile    3.1 1.2 19500101000000  20140919220344  20160531121439  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               YA   AO  4051_7090_089                   2C  D   APEX                            5368                            041511                          846 @�c�]�1   @�dxj?�@3�|�hs�d����o1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    YA   B   B   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�33A�  A���A�  B   B  B  B  B   B(  B0  B:  B>ffBG��BO��BXffB`  Bg��Bp  Bx  B�  B�  B�  B�33B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C�fC  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT�CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� DfD� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&�fD'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt�fDyffD��D�<�D�s3D���D��3D�P D�� D���D�fD�C3D�p D���D���D�0 D�vfD���D�3D�9�D�y�D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�33A�  A���A�  B   B  B  B  B   B(  B0  B:  B>ffBG��BO��BXffB`  Bg��Bp  Bx  B�  B�  B�  B�33B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C�fC  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT�CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� DfD� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&�fD'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt�fDyffD��D�<�D�s3D���D��3D�P D�� D���D�fD�C3D�p D���D���D�0 D�vfD���D�3D�9�D�y�D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��A��A��A䟾A䗍A�+A�hsA�=qA�+A�"�A�"�A��A��A�oA�JA�
=A�%A���A��A��;A���A��;A�VA��;A���A�=qAЮA�A�A��A�dZA�A�A�?}A¸RA��A�9XA���A�VA���A�5?A�jA���A��A��A�7LA�9XA�x�A�bA�ffA�C�A��A��A�Q�A�(�A��A��PA�oA���A��A�M�A�bNA��A�oA�t�A�(�A�x�A��!A��FA�p�A�S�A��-A��A�x�A���A�VA���A��mA���A�n�A��A��A���A���A��A���A�l�A���A�VA���A�~�A�x�A�A�A�JA�%A�\)A�hsA�A��A��^A��/A};dA{+Az�Ax�yAw�At��Ar�RAq|�An��Al(�Aj�Ai33Ag�
Af1'AeVA`��A]dZAZ��AX-AU��AQ��APr�API�AM�AK�AJ�`AI��AH1'AG"�AEVADE�AC��AB�RAA�A@E�A?�A?C�A>9XA;�wA:�HA:v�A:9XA9�A9�FA9`BA9VA8I�A8  A7�A6�/A6VA5�PA4�A4��A4$�A3��A3
=A2=qA0�jA/��A.v�A-`BA,ȴA+�PA*�+A)ƨA(��A'��A'
=A&bA$�+A"��A!l�A!A 5?A;dA5?A;dA��A �A�AVAXA`BA33A�AȴA�AZA(�A��A��AdZAG�A33AoA�RAO�A�FAbA�PA
�9A	�7A	O�A��A��A9XAJA`BA  A�PA�Ax�AhsA��A1A�wA�A��AhsA ~�@��@��R@���@��h@��/@��@��y@���@�x�@�%@��@��P@���@�x�@��u@�o@��@�bN@�@��`@�O�@�h@�Ĝ@�b@�S�@��@�ff@��@���@�;d@���@�"�@��H@ꟾ@�`B@�j@�Z@�ƨ@�^5@��@�V@��T@�7@��@�Ĝ@��@�D@�Z@�1'@���@�~�@�$�@��#@�&�@���@�Q�@�v�@��@ٲ-@�V@�1@�z�@�33@��`@��@��#@���@�Q�@�z�@��@�Ĝ@�9X@Õ�@�x�@�r�@�$�@��`@���@��/@�hs@�V@��9@��m@��@���@��@��H@���@��@�5?@�?}@���@��j@��P@���@��^@��^@��-@��T@�$�@��@�/@��@��@�l�@�33@���@�{@��+@�@�V@���@�7L@�&�@�V@��`@���@���@�1'@��@�t�@��@��@���@�5?@�@��-@��@�7L@��@�Ĝ@��@���@�t�@�\)@�;d@�33@�+@���@��@�^5@�@��^@�hs@�?}@�V@��`@���@�1'@�  @��F@��w@�|�@��@��!@���@��\@�n�@�V@�J@�p�@��@�V@�%@��`@�bN@�Q�@�I�@�9X@�1@��@��m@��m@��m@��m@��m@���@�b@� �@�9X@�bN@�1'@�b@�1@�z�@�9X@���@�t�@�S�@�
=@�o@��P@���@��#@���@�O�@�V@��`@���@��`@��@��D@�r�@�1'@�  @���@��@�@��!@�ff@�E�@��@���@�`B@�/@��@�%@�Ĝ@�Q�@�1'@�(�@�  @�|�@��@��H@���@���@��!@���@���@���@�v�@�$�@���@��@�/@���@�1'@�1@�1@�b@�b@��@�1@��w@�o@���@��@��!@��+@�V@���@���@�`B@�7L@�/@��@���@���@��@��@�S�@���@�$�@��7@�/@�V@���@���@�n�@�K�@���@s��@i�@ax�@Y�@P�`@Fȴ@?��@9�#@2��@,��@&�R@#@�j@  @"�@��@��@	&�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A��A��A��A䟾A䗍A�+A�hsA�=qA�+A�"�A�"�A��A��A�oA�JA�
=A�%A���A��A��;A���A��;A�VA��;A���A�=qAЮA�A�A��A�dZA�A�A�?}A¸RA��A�9XA���A�VA���A�5?A�jA���A��A��A�7LA�9XA�x�A�bA�ffA�C�A��A��A�Q�A�(�A��A��PA�oA���A��A�M�A�bNA��A�oA�t�A�(�A�x�A��!A��FA�p�A�S�A��-A��A�x�A���A�VA���A��mA���A�n�A��A��A���A���A��A���A�l�A���A�VA���A�~�A�x�A�A�A�JA�%A�\)A�hsA�A��A��^A��/A};dA{+Az�Ax�yAw�At��Ar�RAq|�An��Al(�Aj�Ai33Ag�
Af1'AeVA`��A]dZAZ��AX-AU��AQ��APr�API�AM�AK�AJ�`AI��AH1'AG"�AEVADE�AC��AB�RAA�A@E�A?�A?C�A>9XA;�wA:�HA:v�A:9XA9�A9�FA9`BA9VA8I�A8  A7�A6�/A6VA5�PA4�A4��A4$�A3��A3
=A2=qA0�jA/��A.v�A-`BA,ȴA+�PA*�+A)ƨA(��A'��A'
=A&bA$�+A"��A!l�A!A 5?A;dA5?A;dA��A �A�AVAXA`BA33A�AȴA�AZA(�A��A��AdZAG�A33AoA�RAO�A�FAbA�PA
�9A	�7A	O�A��A��A9XAJA`BA  A�PA�Ax�AhsA��A1A�wA�A��AhsA ~�@��@��R@���@��h@��/@��@��y@���@�x�@�%@��@��P@���@�x�@��u@�o@��@�bN@�@��`@�O�@�h@�Ĝ@�b@�S�@��@�ff@��@���@�;d@���@�"�@��H@ꟾ@�`B@�j@�Z@�ƨ@�^5@��@�V@��T@�7@��@�Ĝ@��@�D@�Z@�1'@���@�~�@�$�@��#@�&�@���@�Q�@�v�@��@ٲ-@�V@�1@�z�@�33@��`@��@��#@���@�Q�@�z�@��@�Ĝ@�9X@Õ�@�x�@�r�@�$�@��`@���@��/@�hs@�V@��9@��m@��@���@��@��H@���@��@�5?@�?}@���@��j@��P@���@��^@��^@��-@��T@�$�@��@�/@��@��@�l�@�33@���@�{@��+@�@�V@���@�7L@�&�@�V@��`@���@���@�1'@��@�t�@��@��@���@�5?@�@��-@��@�7L@��@�Ĝ@��@���@�t�@�\)@�;d@�33@�+@���@��@�^5@�@��^@�hs@�?}@�V@��`@���@�1'@�  @��F@��w@�|�@��@��!@���@��\@�n�@�V@�J@�p�@��@�V@�%@��`@�bN@�Q�@�I�@�9X@�1@��@��m@��m@��m@��m@��m@���@�b@� �@�9X@�bN@�1'@�b@�1@�z�@�9X@���@�t�@�S�@�
=@�o@��P@���@��#@���@�O�@�V@��`@���@��`@��@��D@�r�@�1'@�  @���@��@�@��!@�ff@�E�@��@���@�`B@�/@��@�%@�Ĝ@�Q�@�1'@�(�@�  @�|�@��@��H@���@���@��!@���@���@���@�v�@�$�@���@��@�/@���@�1'@�1@�1@�b@�b@��@�1@��w@�o@���@��@��!@��+@�V@���@���@�`B@�7L@�/@��@���@���@��@��@�S�@���@�$�@��7@�/@�V@���G�O�@�n�@�K�@���@s��@i�@ax�@Y�@P�`@Fȴ@?��@9�#@2��@,��@&�R@#@�j@  @"�@��@��@	&�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�B�B�B�
B�
B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��BǮB��B	7BoB@�BYB_;BhsBl�Bm�Br�Bu�Bu�Bs�Bu�Bs�Bv�B�B�B��B�LB�XB�9B�B��B��B�JB� Bx�Bo�Bk�BjBhsBhsBcTB\)BP�B=qB49B.B"�B�B{B1B��B�;B�XB��By�Bm�BdZBW
BQ�BJ�B9XB&�B�B��B�B�B�#BB�B�PB|�Bo�BYBD�B2-B.B'�B�B
��B
�B
�!B
�\B
x�B
ZB
>wB
/B
(�B
�B
\B
B	��B	�B	�yB	�BB	�B	��B	ɺB	��B	�LB	��B	�bB	�B	v�B	jB	\)B	VB	R�B	I�B	A�B	=qB	8RB	2-B	,B	$�B	!�B	�B	�B	oB	PB	
=B	1B	B��B��B��B��B��B��B�B�B�B�B�B�B�sB�`B�TB�NB�;B�/B�#B�
B��B��B��BɺBǮBĜB��B�}B�qB�dB�RB�FB�9B�-B�'B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�3B�LB�LB�LB�LB�qB��B��BBŢBǮB��B��B��B��B��B�B�B�/B�NB�fB�sB�B�B�B��B��B��B�B��B��B��B��B	B	B	B	%B		7B	JB	
=B		7B		7B		7B	DB	JB	VB	�B	�B	�B	�B	�B	�B	 �B	$�B	%�B	(�B	+B	+B	,B	.B	.B	/B	5?B	7LB	7LB	;dB	>wB	A�B	=qB	0!B	.B	.B	1'B	49B	0!B	'�B	!�B	�B	�B	PB	B��B��B��B��B�B�B�B�B�B�B�B��B��B��B��B��B��B	  B	  B	  B	B	B		7B		7B	JB	�B	�B	 �B	%�B	/B	49B	6FB	6FB	49B	5?B	5?B	5?B	6FB	8RB	?}B	B�B	A�B	C�B	G�B	I�B	I�B	H�B	H�B	H�B	G�B	H�B	J�B	L�B	M�B	Q�B	T�B	VB	YB	[#B	\)B	[#B	]/B	^5B	_;B	`BB	`BB	bNB	cTB	e`B	hsB	hsB	iyB	jB	l�B	n�B	o�B	q�B	q�B	t�B	y�B	y�B	{�B	}�B	� B	�B	�B	�B	�B	�B	�B	�+B	�JB	�\B	�bB	�bB	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�?B	�RB	�dB	�}B	ŢB	ƨB	ǮB	ȴB	ȴB	ȴB	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�#B	�#B	�#B	�)B	�/B	�5B	�;B	�;B	�HB	�NB	�TB	�TB	�TB	�ZB	�`B	�`B	�`B	�`B	�fB	�sB	�sB	�sB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B

=B
B
{B
�B
#�B
'�B
/B
8RB
?}B
G�B
M�B
Q�B
XB
]/B
cTB
ffB
k�B
o�B
r�B
w�B
z�B
}�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B�B�B�B�
B�B� B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��BǯB��B	9BsB@�BYB_<BhwBl�Bm�Br�Bu�Bu�Bs�Bu�Bs�Bv�B�B�B��B�SB�aB�@B�B��B��B�RB�Bx�Bo�Bk�Bj�BhxBhwBcYB\,BP�B=vB4<B.B"�B�B}B5B��B�<B�YB��By�Bm�BdYBWBQ�BJ�B9YB&�B�B��B�B�B�#BB�B�PB|�Bo�BYBD�B20B.B'�B�B
��B
�	B
�%B
�cB
x�B
Z#B
>|B
/$B
(�B
�B
eB
B	��B	�B	�B	�IB	�B	��B	��B	��B	�SB	��B	�pB	�%B	v�B	j�B	\7B	VB	R�B	I�B	A�B	=B	8`B	2;B	,B	$�B	!�B	�B	�B	~B	^B	
NB	AB	"B� B��B��B��B��B��B��B�B�B�B��B�B�B�qB�iB�bB�OB�BB�6B�B�	B��B��B��B��BİB��B��B��B�yB�fB�ZB�KB�BB�:B�.B�*B�!B�B�B�B�B�B��B��B��B��B��B�B�B�B�B�	B�	B�B�B�B�B�
B�	B�B��B��B��B��B��B��B��B�B�B�B�FB�`B�aB�^B�`B��B��B��B¤BųBǽB��B��B�	B�	B�B�B�"B�@B�_B�wB�B�B�B�B��B��B��B��B��B��B��B�B	B	"B	(B	2B		HB	XB	
MB		EB		FB		EB	SB	ZB	fB	�B	�B	�B	�B	�B	�B	 �B	$�B	%�B	)B	+B	+B	,B	.!B	.!B	/)B	5MB	7[B	7ZB	;pB	>�B	A�B	=B	0/B	.#B	."B	13B	4FB	0/B	'�B	!�B	�B	�B	`B	B��B��B��B��B�B�B�B�B��B�B��B��B��B��B��B��B�
B	 B	 B	 B	B	-B		DB		FB	UB	�B	�B	 �B	%�B	/&B	4GB	6QB	6RB	4FB	5LB	5KB	5JB	6QB	8]B	?�B	B�B	A�B	C�B	G�B	I�B	I�B	H�B	H�B	H�B	G�B	H�B	J�B	L�B	M�B	Q�B	UB	VB	Y#B	[0B	\5B	[1B	]8B	^@B	_GB	`OB	`OB	b\B	c_B	elB	hB	h~B	i�B	j�B	l�B	n�B	o�B	q�B	q�B	t�B	y�B	y�B	{�B	}�B	�B	�B	�B	�B	�"B	�#B	�(B	�7B	�TB	�dB	�nB	�lB	�sB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�FB	�ZB	�oB	��B	ŪB	ƱB	ǶB	ȻB	ȼB	ȽB	��B	��B	��B	��B	��B	��B	�B	�B	�%B	�*B	�+B	�,B	�0B	�8B	�?B	�CB	�DB	�OB	�VB	�\B	�]B	�[B	�bB	�gB	�hB	�jB	�fB	�oB	�{B	�|B	�|B	�{B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��G�O�B
B
~B
�B
#�B
'�B
/B
8UB
?�B
G�B
M�B
Q�B
XB
]1B
cXB
fkB
k�B
o�B
r�B
w�B
z�B
}�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =0 dbar.                                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311214392016053112143920160531121439  AO  ARCAADJP                                                                    20140919220344    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20140919220344  QCP$                G�O�G�O�G�O�DFB7E           AO  ARGQQCPL                                                                    20140919220344  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531121439  IP                  G�O�G�O�G�O�                