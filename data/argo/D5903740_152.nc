CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-11-21T04:15:19Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         (Matthew Alkire, University of Washington      @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7,   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  74   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7t   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     88   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8X   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8x   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           8|   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~            8�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
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
_FillValue                    g�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  i�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  qt   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    yh   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {h   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �\   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �\   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �P   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �    HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �$   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �(   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �,Argo profile    3.1 1.2 19500101000000  20181121041519  20190604095258  5903740 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  4055                            2C  D   APEX                            5374                            041511                          846 @װ��z1   @װrX�4@;�bM���c|     1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   B   B   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8ffB@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`�Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dts3Dyn�D� D�XRD�t�D��{D�D�7
D��D��D�\D�H�D�l�D���D�{D�J�D�p�D���D�=D�5qD�}�D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�33@�  A  A$  AD  Ad  A�  A�  A�  A�  A�  A�  A�  A�  B  B	  B  B  B!  B)  B1  B9ffBA  BI  BQ  BY  Ba  Bi  Bq  By  B�� B�� B�� B�� B�� B�� B�� B�� B�� B�� B�� B�� B�� B�� B�� B�� B�� BĀ BȀ B̀ BЀ BԀ B؀ B܀ B�� B� B� B� B�� B� B�� B�� C @ C@ C@ C@ C@ C
@ C@ C@ C@ C@ C@ C@ C@ C@ C@ C@ C @ C"@ C$@ C&@ C(@ C*@ C,@ C.@ C0@ C2@ C4@ C6@ C8@ C:@ C<@ C>@ C@@ CB@ CD@ CF@ CH@ CJ@ CL@ CN@ CP@ CR@ CT@ CV@ CX@ CZ@ C\@ C^@ C`Y�Cb@ Cd@ Cf@ Ch@ Cj@ Cl@ Cn@ Cp@ Cr@ Ct@ Cv@ Cx@ Cz@ C|@ C~@ C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D  D � D D� D D� D D� D D� D D� D D� D D� D D� D	 D	� D
 D
� D D� D D� D D� D D� D D� D D� D D� D D� D D� D D� D D� D D� D D� D D� D D� D D� D D� D D� D D� D D� D D� D  D � D! D!� D" D"� D# D#� D$ D$� D% D%� D& D&� D' D'� D( D(� D) D)� D* D*� D+ D+� D, D,� D- D-� D. D.� D/ D/� D0 D0� D1 D1� D2 D2� D3 D3� D4 D4� D5 D5� D6 D6� D7 D7� D8 D8� D9 D9� D: D:� D; D;� D< D<� D= D=� D> D>� D? D?� D@ D@� DA DA� DB DB� DC DC� DD DD� DE DE� DF DF� DG DG� DH DH� DI DI� DJ DJ� DK DK� DL DL� DM DM� DN DN� DO DO� DP DP� DQ DQ� DR DR� DS DS� DT DT� DU DU� DV DV� DW DW� DX DX� DY DY� DZ DZ� D[ D[� D\ D\� D] D]� D^ D^� D_ D_� D` D`� Da Da� Db Db� Dc Dc� Dd Dd� De De� Df Df� Dg Dg� Dh Dh� Di Di� Dj Dj� Dk Dk� Dl Dl� Dm Dm� Dn Dn� Do Do� Dp Dp� Dq Dq� Dr Dr� Ds Ds� Dt Dt�3Dy~�D� D�`RD�|�D��{D�D�?
D��D��D�\D�P�D�t�D���D�{D�R�D�x�D���D�
=D�=qD��D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��HA��A��A��A��A��A��A��A��`A��yA��A���A���A���A� �A��\A���A� �A�ȴA�t�A�%A�JA�+A�  A���A�K�A��;A��+A�9XA�C�A���A�XA���A� �A�Q�A���A���A�S�A�p�A���A�-A��+A�ȴA�=qA�t�A��A�/A�;dA���A���A�r�A���A��A���A��mA��jA���A��jA�ffA��A��A��yA�%A��!A��^A�33A��A�
=A��A���A�$�A�O�A��A���A���A��9A�&�A�bA��hA��mA�A�`BA��7A�S�A��A�ȴA��A�M�A�%A���A�%A���A�-A�r�A��A�?}A��`A���A�%A�dZA�9XA��jA�O�A���A�+A���A�1A���A}�wAzjAv��ArbNAq7LApVAo�An�jAn~�AmAm��Al�!Ak��Ai�Ag�
Af��Ae�Ac�A`��A_�A^JA]�A]�#A]%AZ�AX��AWt�AV~�AU�#AT��AT^5ASVAP$�AN~�AL�AK"�AI�#AH��AH(�AGC�AFAE|�AEoAD  AC��ACG�AA�-A@~�A?��A?p�A>$�A<��A;��A;&�A9�wA8�A7��A7VA6ȴA6(�A5�-A5`BA57LA4�A3�TA21'A1l�A0�!A/��A.z�A.1A-�A+�
A+hsA*ĜA*bA)�A'�mA'�A'oA&��A%\)A$�jA#O�A!�
A ȴA
=A(�A�A��A�
A��A�9A�TA�A?}A%A�yA�HA��A��Az�A=qAA�yA�+A��A�AjA��A`BA��AVA��Ax�AI�AG�A�A�#Al�A
�A	��A�AK�A�yA�A�A�/A  A?}A�+A��A ��A {@�ȴ@���@�o@�?}@�1'@�n�@�O�@��u@�@�hs@�9X@��H@@�@�b@�+@�E�@�%@�A�@�l�@��H@���@�^5@�`B@�D@�1'@��
@�33@�^5@�X@�ƨ@�J@�1@�ȴ@��#@�`B@؃@�dZ@�O�@Ӿw@�-@��@�j@��;@��y@���@�X@˶F@�~�@���@�/@���@�Q�@�ƨ@�o@�V@�`B@�9X@+@���@���@�j@�t�@�
=@��@��\@�$�@�G�@�j@��
@�+@��y@��#@�z�@�bN@�9X@�"�@�@�X@� �@��P@��@�~�@���@�A�@��@��m@�ƨ@��P@�^5@�G�@��`@���@�I�@�"�@�$�@���@��@���@�&�@�Z@�b@���@�dZ@�n�@���@�%@��F@�\)@�33@�~�@�=q@��^@�&�@��@�z�@�I�@���@�t�@���@�-@�J@���@���@���@�l�@��!@�5?@���@�7L@��@��@��@���@�~�@�{@�@��@���@�V@��j@�bN@�I�@� �@��;@���@���@�l�@�+@���@���@�v�@�-@�{@��T@��^@���@��7@�X@�G�@�V@��u@�1'@���@�\)@��@�n�@�-@��@��@��7@�`B@�&�@�Ĝ@�1'@��
@��w@��P@�S�@��@��@��\@�n�@�=q@���@���@���@��@�G�@���@��/@��@��D@�Q�@�b@���@��@��P@�dZ@�
=@�v�@�{@��-@�p�@�?}@�7L@���@���@�Ĝ@��j@���@�z�@�9X@� �@�1@���@��m@��;@���@���@���@�dZ@��H@��\@�^5@�-@��@�@�@��-@���@��7@�`B@�G�@���@�Ĝ@���@��D@�Z@�1'@��@�1@�w@;d@�@~�y@{s@s��@j5?@b��@XC-@T>B@N�@Hj@A��@9�o@3�@.�@)p�@$�.@��@�9@,=@�_@�@	w2@�M11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   A��HA��A��A��A��A��A��A��A��`A��yA��A���A���A���A� �A��\A���A� �A�ȴA�t�A�%A�JA�+A�  A���A�K�A��;A��+A�9XA�C�A���A�XA���A� �A�Q�A���A���A�S�A�p�A���A�-A��+A�ȴA�=qA�t�A��A�/A�;dA���A���A�r�A���A��A���A��mA��jA���A��jA�ffA��A��A��yA�%A��!A��^A�33A��A�
=A��A���A�$�A�O�A��A���A���A��9A�&�A�bA��hA��mA�A�`BA��7A�S�A��A�ȴA��A�M�A�%A���A�%A���A�-A�r�A��A�?}A��`A���A�%A�dZA�9XA��jA�O�A���A�+A���A�1A���A}�wAzjAv��ArbNAq7LApVAo�An�jAn~�AmAm��Al�!Ak��Ai�Ag�
Af��Ae�Ac�A`��A_�A^JA]�A]�#A]%AZ�AX��AWt�AV~�AU�#AT��AT^5ASVAP$�AN~�AL�AK"�AI�#AH��AH(�AGC�AFAE|�AEoAD  AC��ACG�AA�-A@~�A?��A?p�A>$�A<��A;��A;&�A9�wA8�A7��A7VA6ȴA6(�A5�-A5`BA57LA4�A3�TA21'A1l�A0�!A/��A.z�A.1A-�A+�
A+hsA*ĜA*bA)�A'�mA'�A'oA&��A%\)A$�jA#O�A!�
A ȴA
=A(�A�A��A�
A��A�9A�TA�A?}A%A�yA�HA��A��Az�A=qAA�yA�+A��A�AjA��A`BA��AVA��Ax�AI�AG�A�A�#Al�A
�A	��A�AK�A�yA�A�A�/A  A?}A�+A��A ��A {@�ȴ@���@�o@�?}@�1'@�n�@�O�@��u@�@�hs@�9X@��H@@�@�b@�+@�E�@�%@�A�@�l�@��H@���@�^5@�`B@�D@�1'@��
@�33@�^5@�X@�ƨ@�J@�1@�ȴ@��#@�`B@؃@�dZ@�O�@Ӿw@�-@��@�j@��;@��y@���@�X@˶F@�~�@���@�/@���@�Q�@�ƨ@�o@�V@�`B@�9X@+@���@���@�j@�t�@�
=@��@��\@�$�@�G�@�j@��
@�+@��y@��#@�z�@�bN@�9X@�"�@�@�X@� �@��P@��@�~�@���@�A�@��@��m@�ƨ@��P@�^5@�G�@��`@���@�I�@�"�@�$�@���@��@���@�&�@�Z@�b@���@�dZ@�n�@���@�%@��F@�\)@�33@�~�@�=q@��^@�&�@��@�z�@�I�@���@�t�@���@�-@�J@���@���@���@�l�@��!@�5?@���@�7L@��@��@��@���@�~�@�{@�@��@���@�V@��j@�bN@�I�@� �@��;@���@���@�l�@�+@���@���@�v�@�-@�{@��T@��^@���@��7@�X@�G�@�V@��u@�1'@���@�\)@��@�n�@�-@��@��@��7@�`B@�&�@�Ĝ@�1'@��
@��w@��P@�S�@��@��@��\@�n�@�=q@���@���@���@��@�G�@���@��/@��@��D@�Q�@�b@���@��@��P@�dZ@�
=@�v�@�{@��-@�p�@�?}@�7L@���@���@�Ĝ@��j@���@�z�@�9X@� �@�1@���@��m@��;@���@���@���@�dZ@��H@��\@�^5@�-@��@�@�@��-@���@��7@�`B@�G�@���@�Ĝ@���@��D@�Z@�1'@��@�1@�w@;d@�G�O�@{s@s��@j5?@b��@XC-@T>B@N�@Hj@A��@9�o@3�@.�@)p�@$�.@��@�9@,=@�_@�@	w2@�M11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB\B\B\B\B\B\B\BVBVBVBVBVBVBJBJBPBhBoB+B0!B1'B1'B33B0!B'�B"�B�B�B�BDB��B��B��B�B��B�B��B��B�B�B�B�sB�
B��B�qB�B�?B�B��B��B�DB�Bu�Bk�BcTB_;BT�BG�B<jB>wB@�BI�BZBVBJ�BG�B@�B49B5?B-B �B�BVB	7B1B	7BB�B�sB�B�B��BÖB�!B��B�B��B��B�oB�JB�B{�Br�BgmB^5BQ�BJ�BC�B8RB,B�BbB%B
��B
�B
�TB
��B
�B
z�B
N�B
#�B	��B	�yB	�B	�B	�ZB	�HB	�
B	��B	��B	ɺB	�RB	�B	��B	��B	�1B	x�B	k�B	ffB	gmB	o�B	k�B	ZB	L�B	F�B	B�B	=qB	8RB	7LB	0!B	 �B	�B	VB	+B	B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�sB�`B�BB�B��B��B��BɺBŢBÖB��B�}B�jB�?B�B��B��B��B��B��B��B�uB�hB�\B�JB�1B�B�B�B}�By�Bu�Bp�Bl�BjBcTBaHBaHB`BB^5B[#BYBW
BVBVBVBT�BT�BT�BS�BS�BR�BP�BO�BM�BJ�BH�BG�BF�BE�BD�BC�BB�B@�B=qB:^B8RB5?B49B2-B0!B.B,B+B)�B'�B&�B%�B$�B"�B!�B#�B#�B �B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B �B �B!�B!�B �B �B!�B!�B"�B$�B$�B$�B$�B&�B%�B'�B(�B(�B(�B(�B(�B)�B)�B+B,B-B0!B49B49B49B5?B7LB6FB7LB6FB7LB:^B;dB;dB:^B;dB>wB>wB=qB@�BC�BD�BH�BI�BH�BJ�BM�BP�BP�BQ�BQ�BP�BT�BXBZBZBZB]/B_;B_;B`BB`BBaHBdZBe`Be`BhsBl�Bn�Bo�Bt�Bu�Bu�Bx�Bx�Bz�B}�B~�B� B� B�B�B�B�+B�1B�1B�VB�oB��B��B��B��B��B��B��B�B�B�!B�3B�3B�9B�9B�XB�dB�wB�wB��B��BÖBÖBĜBƨBǮBɺB��B��B��B��B��B��B��B��B��B�B�B�/B�HB�NB�mB�B�B�B�B�B��B��B��B��B	B	B	B	%B	1B		7B	PB	\B	oB	�B	�B	�B	�B	�B	�B	 �B	#�B	%�B	&�B	(�B	+B	,B	-B	.B	0!B	49B	7LB	;dB	=qB	B�B	D�B	H�B	J�B	K�B	K�B	L�B	M�B	P�B	Q�B	Q�B	R�B	S�B	S�B	T�B	VB	VB	W
B	[#B	]/B	^5B	`BB	dZB	gmB	gmB	hsB	jB	k�B	m�B	o�B	q�B	s�B	t�B	v�B	x�B	y�B	z�B	z�B	{�B	~�B	� B	�B	�vB	�[B	ՁB	�B	�B	�B
DB
�B
�B
(>B
4B
<�B
C{B
IB
Q4B
X�B
^�B
c�B
h
B
m�B
q[11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   BGBHBGBEBFBGBGB@B@B@B<B@BDB5B5B9BTB[B*�B0B1B1B3B0
B'�B"�B�B�BpB-B��B��B��B�B��B�B��B��B�B�B�B�^B��BʫB�]B�B�(B�B��B��B�,B��Bu�BknBc;B_'BT�BG�B<UB>^B@kBI�BZBU�BJ�BG�B@jB4#B5'B,�B �BrB;B	BB	!B �B�B�WB��B��BοB�|B�
B��B�B��B�wB�WB�3B��B{�Br�BgRB^BQ�BJ�BCB89B+�B�BHBB
��B
�{B
�9B
ͷB
�B
z�B
N�B
#�B	��B	�_B	�~B	�eB	�AB	�-B	��B	��B	��B	ɟB	�7B	��B	��B	�lB	�B	x�B	kiB	fLB	gSB	o�B	kkB	ZB	L�B	F�B	BtB	=WB	87B	7/B	0B	 �B	rB	9B	B	�B��B��B��B��B��B��B��B��B��B�B�nB�tB�B�vB�UB�FB�(B��B��B͸B˫BɟBńB�{B�mB�aB�OB�$B��B��B��B��B��B��B�lB�ZB�KB�=B�-B�B��B��B��B}�By�Bu�Bp�BloBjaBc7Ba,Ba*B`&B^B[BX�BV�BU�BU�BU�BT�BT�BT�BS�BS�BR�BP�BO�BM�BJ�BH�BG�BF�BE�BD~BCyBBrB@eB=RB:@B85B5!B4B2B0B-�B+�B*�B)�B'�B&�B%�B$�B"�B!�B#�B#�B �B�B�B�B�B�B�B�BsBjBnBmBiBjBhBhBhBnBrBwBsBrBtBsBrB}B�B{BzB|B�B�B �B �B!�B!�B �B �B!�B!�B"�B$�B$�B$�B$�B&�B%�B'�B(�B(�B(�B(�B(�B)�B)�B*�B+�B,�B0B4B4B4B5 B7)B6'B7-B6&B7,B:>B;CB;EB:=B;CB>WB>XB=SB@dBCvBDzBH�BI�BH�BJ�BM�BP�BP�BQ�BQ�BP�BT�BW�BY�BY�BY�B]B_B_B`"B`"Ba'Bd9BeBBe@BhSBljBnxBoBt�Bu�Bu�Bx�Bx�Bz�B}�B~�B�B�B��B��B��B�	B�B�B�5B�NB�`B�nB��B��B��B��B��B��B��B� B�B�B�B�B�8B�DB�UB�UB�bB�hB�vB�tB�}BƇBǌBɚBˤBʹBκB��B��B��B��B��B��B��B��B�B�)B�,B�MB�fB�oB�vB�|B�B��B��B��B��B	�B	�B	�B	B	B		B	-B	;B	MB	_B	iB	rB	yB	yB	�B	 �B	#�B	%�B	&�B	(�B	*�B	+�B	,�B	-�B	0 B	4B	7+B	;@B	=QB	BlB	DyB	H�B	J�B	K�B	K�B	L�B	M�B	P�B	Q�B	Q�B	R�B	S�B	S�B	T�B	U�B	U�B	V�B	[B	]B	^B	`!B	d9B	gJB	gJB	hRB	j`B	kdB	mqB	o{B	q�B	s�B	t�B	v�B	x�B	y�B	z�B	z�B	{�B	~�B	�G�O�B	�UB	�;B	�bB	�B	�B	��B
"B
�B
�B
(B
3�B
<�B
C[B
H�B
QB
X�B
^~B
c�B
g�B
m�B
q;11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.25 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                                                                                                             201906040952582019060409525820190604095258  AO  ARCAADJP                                                                    20181121041519    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181121041519  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181121041519  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20190604095259  IP                  G�O�G�O�G�O�                