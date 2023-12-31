CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-06-06T19:16:50Z AOML 3.0 creation; 2016-08-07T21:17:37Z UW 3.1 conversion     
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
_FillValue                 �  A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  Cx   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  KX   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  MP   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  U0   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ]   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  _   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  f�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  h�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  p�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  x�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  z�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �x   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �p   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
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
_FillValue                    �,Argo profile    3.1 1.2 19500101000000  20150606191650  20160807141737  5904460 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               6A   AO  5285_8895_054                   2C  D   APEX                            6487                            072314                          846 @�VkX��1   @�Vl�8��@.��Q��c�ȴ9X1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    6A   B   B   @333@�  @�  A   A   AA��A`  A�  A�  A�  A�  A�  A�  A�  A�  B   BffBffB  B��B(  B0  B8  B@  BHffBP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp�Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt�fDy� D��3D�<�D��3D�� D�fD�FfD��fD�� D��D�FfD�y�D��fD��3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@HQ�@��\@ʏ\AG�A%G�AF�HAeG�A���A���A���A���A£�Aң�A��A��BQ�B	�RB�RBQ�B �B)Q�B1Q�B9Q�BAQ�BI�RBQQ�BYQ�BaQ�BiQ�BqQ�ByQ�B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���BĨ�BȨ�B̨�BШ�B��)Bب�Bܨ�B��B��B��B��B��B���B���B���C :�CT{CT{CT{CT{C
T{CT{CT{CT{CT{CT{CT{CT{CT{CT{CT{C T{C"T{C$T{C&T{C(T{C*T{C,T{C.T{C0T{C2T{C4T{C6T{C8T{C:T{C<T{C>T{C@T{CBT{CDT{CFT{CHT{CJT{CLT{CNT{CPT{CRT{CTT{CVT{CXT{CZT{C\T{C^T{C`T{CbT{CdT{CfT{ChT{CjT{ClT{CnT{CpnCrT{CtT{CvT{CxT{CzT{C|T{C~T{C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�7
C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=D D �DD�DD�DD�DD�DD�DD�DD�DD�D	D	�D
D
�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�D D �D!D!�D"D"�D#D#�D$D$�D%D%�D&D&�D'D'�D(D(�D)D)�D*D*�D+D+�D,D,�D-D-�D.D.�D/D/�D0D0�D1D1�D2D2�D3D3�D4D4�D5D5�D6D6�D7D7�D8D8�D9D9�D:D:�D;D;�D<D<�D=D=�D>D>�D?D?�D@D@�DADA�DBDB�DCDC�DDDD�DEDE�DFDF�DGDG�DHDH�DIDI�DJDJ�DKDK�DLDL�DMDM�DNDN�DODO�DPDP�DQDQ�DRDR�DSDS�DTDT�DUDU�DVDV�DWDW�DXDX�DYDY�DZDZ�D[D[�D\D\�D]D]�D^D^�D_D_�D`D`�DaDa�DbDb�DcDc�DdDd�DeDe�DfDf�DgDg�DhDh�DiDi�DjDj�DkDk�DlDl�DmDm�DnDn�DoDo�DpDp�DqDq�DrDr�DsDs�DtDt�DtۅDy�D���D�G\D���D�ڏD��D�P�D���D�ʏD�'\D�P�D��)D���D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�=qA�A�A�A�A�?}A�E�A�A�A�A�A�A�A�E�A�G�A�G�A�I�A�G�A�C�A�C�A�E�A�=qA�;dA�+A��A�dZA�=qA�x�A���A�"�A���A˲-A�;dAɴ9A�ffA�&�AǁA�AƋDA�A�AŶFA�%A�XA���A�?}A�A�-A���A��\A��RA���A��-A�^5A��!A��A�?}A���A�  A�~�A�G�A��PA�5?A�K�A�A�=qA�ZA���A��A��A�%A�;dA�O�A��FA��RA��;A�VA�JA��A�ĜA�%A���A���A��HA�VA�dZA��uA�|�A�ȴA��A�$�A�7LA��HA��TA�A���A�hsA���A��A�?}A��jA�{A�$�Aw�-Ar^5Al�/Ai�7Acp�AY�AW�^AV1AR�jAP�AMC�AI�AH$�AF9XAA\)A?�TA=�TA<v�A:Q�A9;dA7�mA7�A6�RA5?}A3�
A2�HA2�uA1�A0ĜA/K�A/
=A.�RA-�;A-t�A-C�A+�hA(Q�A$ffA%
=A%K�A&�\A'?}A'dZA(  A(ffA(n�A'�wA'A'\)A&1'A%p�A$�/A#�wA#&�A"��A"(�A!��A!�A!;dA!A �DA�Ax�A��AjAA�7A�`AI�AJAA"�A�A�A��AVA�PAG�A%A�AI�A�A-AJA��AƨAVA��A��AG�A�A=qAAbA��A�FAK�A�jA5?AA��A��A��AA�A�PAS�A�A��A5?A�A��AO�AAȴAv�A��A
=A
�/A
��A
��A
A�A	�^A	/A�A�HA�!Ar�A5?A�
A��AAv�A  A��A�A�9AjAZAZAE�A�;A`BA�HA~�A5?A�;A��At�A+A �`A ��A  �@�"�@�M�@�J@��T@�7L@���@�  @��m@��@��!@��#@��h@�p�@�1'@�"�@�E�@���@�bN@�  @�K�@�M�@�p�@�&�@��@���@�Ĝ@�u@�bN@�l�@��T@��@�1'@�Q�@�1@�C�@���@�R@�\@�M�@�{@���@�p�@�b@�C�@��H@�ff@�-@�O�@��@��@�j@�9X@㕁@�;d@�+@�{@�O�@�Ĝ@�bN@��
@߅@�\)@�v�@�-@ݲ-@�x�@��@��@�ƨ@���@���@�o@ڇ+@�V@�-@��#@ى7@�hs@�O�@؋D@�ƨ@�l�@�l�@�;d@�ȴ@�5?@�@���@��@��H@�V@�J@�p�@ЋD@� �@��;@�ƨ@Ͼw@ϝ�@�dZ@�+@��y@ΰ!@�@�?}@���@�I�@�(�@� �@�  @�|�@�5?@�X@��@�r�@�1'@Ǖ�@��y@Ə\@�5?@��@ř�@�&�@ě�@�A�@�ƨ@��@�@�-@��@��u@��@�+@�ȴ@���@�n�@�$�@���@���@���@�p�@�V@�A�@�1@��F@�C�@�$�@��/@�  @��@��@���@���@�hs@�/@�%@��@��P@�l�@�@��R@��#@�X@��/@�Q�@���@��m@��
@��
@��@�+@���@�ff@�M�@�-@�@�@�G�@���@��9@��u@�j@�  @���@��@�M�@���@���@���@�1'@���@�ƨ@���@�S�@�"�@��!@�n�@�^5@�V@�M�@�=q@�M�@�=q@�{@���@���@�p�@��@���@��j@��j@�z�@�(�@���@�K�@�o@��@�~�@�@��T@��h@��@���@�bN@���@�C�@��@��y@��\@�-@��@�$�@�`B@���@�bN@�  @��m@��F@�t�@�"�@�?}@��\@��@�v�@�^5@|��@s��@m�-@e�@]?}@U?}@J�@C"�@<�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111A�=qA�A�A�A�A�?}A�E�A�A�A�A�A�A�A�E�A�G�A�G�A�I�A�G�A�C�A�C�A�E�A�=qA�;dA�+A��A�dZA�=qA�x�A���A�"�A���A˲-A�;dAɴ9A�ffA�&�AǁA�AƋDA�A�AŶFA�%A�XA���A�?}A�A�-A���A��\A��RA���A��-A�^5A��!A��A�?}A���A�  A�~�A�G�A��PA�5?A�K�A�A�=qA�ZA���A��A��A�%A�;dA�O�A��FA��RA��;A�VA�JA��A�ĜA�%A���A���A��HA�VA�dZA��uA�|�A�ȴA��A�$�A�7LA��HA��TA�A���A�hsA���A��A�?}A��jA�{A�$�Aw�-Ar^5Al�/Ai�7Acp�AY�AW�^AV1AR�jAP�AMC�AI�AH$�AF9XAA\)A?�TA=�TA<v�A:Q�A9;dA7�mA7�A6�RA5?}A3�
A2�HA2�uA1�A0ĜA/K�A/
=A.�RA-�;A-t�A-C�A+�hA(Q�A$ffA%
=A%K�A&�\A'?}A'dZA(  A(ffA(n�A'�wA'A'\)A&1'A%p�A$�/A#�wA#&�A"��A"(�A!��A!�A!;dA!A �DA�Ax�A��AjAA�7A�`AI�AJAA"�A�A�A��AVA�PAG�A%A�AI�A�A-AJA��AƨAVA��A��AG�A�A=qAAbA��A�FAK�A�jA5?AA��A��A��AA�A�PAS�A�A��A5?A�A��AO�AAȴAv�A��A
=A
�/A
��A
��A
A�A	�^A	/A�A�HA�!Ar�A5?A�
A��AAv�A  A��A�A�9AjAZAZAE�A�;A`BA�HA~�A5?A�;A��At�A+A �`A ��A  �@�"�@�M�@�J@��T@�7L@���@�  @��m@��@��!@��#@��h@�p�@�1'@�"�@�E�@���@�bN@�  @�K�@�M�@�p�@�&�@��@���@�Ĝ@�u@�bN@�l�@��T@��@�1'@�Q�@�1@�C�@���@�R@�\@�M�@�{@���@�p�@�b@�C�@��H@�ff@�-@�O�@��@��@�j@�9X@㕁@�;d@�+@�{@�O�@�Ĝ@�bN@��
@߅@�\)@�v�@�-@ݲ-@�x�@��@��@�ƨ@���@���@�o@ڇ+@�V@�-@��#@ى7@�hs@�O�@؋D@�ƨ@�l�@�l�@�;d@�ȴ@�5?@�@���@��@��H@�V@�J@�p�@ЋD@� �@��;@�ƨ@Ͼw@ϝ�@�dZ@�+@��y@ΰ!@�@�?}@���@�I�@�(�@� �@�  @�|�@�5?@�X@��@�r�@�1'@Ǖ�@��y@Ə\@�5?@��@ř�@�&�@ě�@�A�@�ƨ@��@�@�-@��@��u@��@�+@�ȴ@���@�n�@�$�@���@���@���@�p�@�V@�A�@�1@��F@�C�@�$�@��/@�  @��@��@���@���@�hs@�/@�%@��@��P@�l�@�@��R@��#@�X@��/@�Q�@���@��m@��
@��
@��@�+@���@�ff@�M�@�-@�@�@�G�@���@��9@��u@�j@�  @���@��@�M�@���@���@���@�1'@���@�ƨ@���@�S�@�"�@��!@�n�@�^5@�V@�M�@�=q@�M�@�=q@�{@���@���@�p�@��@���@��j@��j@�z�@�(�@���@�K�@�o@��@�~�@�@��T@��h@��@���@�bN@���@�C�@��@��y@��\@�-@��@�$�@�`B@���@�bN@�  @��m@��F@�t�G�O�@�?}@��\@��@�v�@�^5@|��@s��@m�-@e�@]?}@U?}@J�@C"�@<�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;oB	�hB	�hB	�hB	�hB	�hB	�hB	�hB	�hB	�hB	�hB	�hB	�hB	�hB	�hB	�hB	�hB	�oB	�oB	�hB	�bB	�hB	�\B	�B	�oB	�BB
dZBBe`B��B�mB6FBD�BL�BQ�BP�BW
BYBgmBq�B~�B�=B��B�BB�)B�B��BB
=B\B�B �B!�B2-B"�B  B�B�HB�/B�)B��B�RB�B��B��B�JB�Bt�BiyB\)BS�BVBK�B?}B/B,B"�B �B{B%B��B��B�-B��Br�B>wB�BB
�B
�dB
�B
s�B
ffB
YB
L�B
9XB
B	��B	�uB	n�B	Q�B	+B��B�B�fB�/B�
B��B��B��B��B�yB�B�B��B	DB	JB	VB	&�B	9XB	6FB	<jB	K�B	r�B	�B	~�B	x�B	�B	��B	�B	�B	�'B	��B	�{B	�B	��B	�XB	�#B	�B	��B
1B
�B
 �B
�B
$�B
$�B
 �B
'�B
-B
33B
49B
5?B
2-B
5?B
8RB
8RB
:^B
9XB
9XB
:^B
:^B
;dB
<jB
<jB
<jB
;dB
;dB
>wB
<jB
@�B
B�B
A�B
@�B
=qB
<jB
:^B
8RB
5?B
6FB
?}B
@�B
@�B
?}B
?}B
>wB
;dB
<jB
=qB
<jB
;dB
;dB
;dB
;dB
;dB
;dB
:^B
:^B
:^B
9XB
8RB
8RB
7LB
6FB
7LB
5?B
49B
49B
49B
33B
49B
49B
33B
33B
2-B
1'B
1'B
1'B
0!B
0!B
0!B
.B
.B
0!B
1'B
1'B
0!B
/B
-B
+B
)�B
(�B
(�B
(�B
'�B
(�B
,B
-B
,B
+B
)�B
&�B
%�B
$�B
'�B
'�B
&�B
%�B
$�B
#�B
"�B
"�B
!�B
!�B
 �B
�B
#�B
"�B
"�B
"�B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
{B
uB
{B
{B
{B
{B
{B
{B
{B
uB
oB
bB
bB
bB
oB
uB
hB
hB
hB
oB
oB
hB
hB
bB
\B
\B
VB
VB
VB
PB
PB
PB
PB
JB
DB
DB

=B
	7B
	7B

=B

=B
1B
1B
1B
	7B

=B
DB
DB

=B
1B
1B
1B
	7B
	7B
	7B
1B
1B
1B
+B
+B
+B
%B
%B
%B
%B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
+B
+B
1B
	7B
1B
	7B
	7B
1B
1B
1B
+B
B
B
B
B
B
B
B
%B
+B
+B
+B
1B
1B
	7B
	7B

=B

=B

=B

=B
	7B
	7B
	7B
1B
1B
1B
+B
+B
+B
+B
1B
1B
1B
+B
1B
1B
1B
1B
+B
1B
1B

=B
JB
VB
VB
\B
\B
\B
\B
\B
\B
\B
bB
bB
bB
hB
oB
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
hB
hB
oB
oB
oB
oB
oB
uB
�B
uB
uB
uB
{B
{B
�B
�B
�B
�B
$�B
'�B
.B
33B
8RB
<jB
B�B
H�B
J�B
N�B
S�B
YB
_;111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111B	�YB	�[B	�[B	�[B	�[B	�ZB	�[B	�ZB	�\B	�\B	�]B	�ZB	�\B	�ZB	�[B	�\B	�`B	�bB	�[B	�UB	�[B	�MB	��B	�bB	�2B
dEB�BeFB��B�QB6,BD�BL�BQ�BP�BV�BX�BgRBq�B~�B�#B�vB��B�xB�B�fB��B�B
!BBB�B �B!�B2B"�B��B�hB�+B�B�BʤB�8B��B��B�hB�.B��Bt�BiXB\BS�BU�BK�B?]B.�B+�B"�B �B^BB��B��B�B�nBr�B>YBmB�B
�zB
�FB
��B
s�B
fGB
X�B
L�B
9<B
�B	�mB	�^B	n�B	Q�B	*�B��B�B�UB�B��B��B̼B��B��B�hB�~B�B��B	0B	4B	?B	&�B	9BB	6/B	<TB	K�B	r�B	��B	~�B	x�B	�B	��B	��B	��B	�
B	��B	�`B	��B	��B	�<B	�B	��B	��B
B
yB
 �B
�B
$�B
$�B
 �B
'�B
,�B
3B
4B
5B
2B
5B
83B
82B
:<B
96B
97B
:=B
:=B
;DB
<IB
<IB
<HB
;EB
;AB
>UB
<JB
@`B
BmB
AhB
@cB
=NB
<KB
::B
80B
5B
6$B
?[B
@aB
@aB
?]B
?ZB
>XB
;CB
<EB
=QB
<IB
;CB
;@B
;CB
;BB
;AB
;BB
:;B
:<B
:;B
94B
80B
82B
7)B
6$B
7*B
5B
4B
4B
4B
3B
4B
4B
3B
3B
2B
1B
1B
1B
/�B
/�B
/�B
-�B
-�B
0 B
1B
1B
/�B
.�B
,�B
*�B
)�B
(�B
(�B
(�B
'�B
(�B
+�B
,�B
+�B
*�B
)�B
&�B
%�B
$�B
'�B
'�B
&�B
%�B
$�B
#�B
"�B
"�B
!�B
!�B
 �B
�B
#�B
"�B
"�B
"�B
 �B
�B
�B
�B
|B
xB
rB
eB
]B
XB
TB
WB
ZB
YB
[B
XB
YB
XB
SB
KB
?B
@B
?B
MB
RB
EB
CB
DB
LB
MB
EB
EB
?B
:B
9B
3B
3B
2B
.B
-B
*B
.B
)B
#B
 B

B
	B
	B

B

B
B
B
B
	B

B
 B
"B

B
B
B
B
	B
	B
	B
B
B
B
B
B
	B
B
B
B
B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
 �B
 �B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
 �B
 �B
 �B
 �B
 �B
 �B
 �B
 �B
�B
�B
 B
B
 B
B
B
B
	B
B
	B
	B
B
B
B
	B
�B
�B
�B
�B
�B
�B
�B
B
B
B

B
B
B
	B
	B

B

B

B

B
	B
	B
	B
B
B
B
B
B
B
	B
B
B
B

B
B
B
B
B
B
B
B

B
&B
3B
2B
9B
7B
7B
8B
8B
8B
6B
?B
?B
>B
CB
KB
CB
BB
CB
IB
HB
JB
IB
LB
IB
IB
DB
BB
JB
KB
IB
HB
IB
OB
[B
PB
QB
RB
UB
UB
cB
pG�O�B
�B
$�B
'�B
-�B
3B
8/B
<CB
BfB
H�B
J�B
N�B
S�B
X�B
_111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.33 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071417372016080714173720160807141737  AO  ARCAADJP                                                                    20150606191650    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150606191650  QCP$                G�O�G�O�G�O�0               AO  ARGQQCPL                                                                    20150606191650  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807141737  IP                  G�O�G�O�G�O�                