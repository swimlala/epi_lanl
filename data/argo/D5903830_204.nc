CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   8   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-11-21T05:28:46Z creation      
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
resolution        =���   axis      Z         �  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  8  :P   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���      �  :�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  8  ;h   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���      �  ;�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o      �  <�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  8  =`   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o      �  =�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  8  >x   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o      �  >�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o      �  ?�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  8  @p   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o      �  @�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  8  A�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o      �  A�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  B�   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    B�   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    E�   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    H�   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  K�   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    K�   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    L    HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    L   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    L   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  L   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    LL   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    L\   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    L`   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         Lp   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         Lt   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        Lx   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    L|Argo profile    3.1 1.2 19500101000000  20181121052846  20200819175744  5903830 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   PRES            TEMP            PSAL               �A   AO  4257                            2C  D   SOLO                            3031                            SB6.01  6   11                  851 @�f��7�1   @�j9D�B@9DZ�1�dГt�j1   ARGOS   Primary sampling: averaged []                                                                                                                                                                                                                                      A   A   A   @�  A  Ap  A�  A�  B  B  B4  BL  Bd  B|  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C  C  C  C  C  C  C%  C+  C1  C7  CI  Cg  C�� C�� C�� C�� C�� C̀ C܀ C� C�� D� D@ D� D  D.  D=  DL  D[  Dj  Dy  D�  D�� D�  D�� 11111111111111111111111111111111111111111111111111111111@�  A  Ap  A�  A�  B  B  B4  BL  Bd  B|  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C  C  C  C  C  C  C%  C+  C1  C7  CI  Cg  C�� C�� C�� C�� C�� C̀ C܀ C� C�� D� D@ D� D  D.  D=  DL  D[  Dj  Dy  D�  D�� D�  D�� 11111111111111111111111111111111111111111111111111111111?���?�)�?�|�?��?�!�?�t??���?�e?�k�?ʾ�?�?�c�?˶F?��?�[l?̮ ?� �?�S&?ͥ�?��M?�J�?Νs?��?�B�?ϕ-?���?�:T?Ќ�?��z?�2?ф�?�|[?�;?ն?�R�?���?ڌ�?�)�?��~?�c^?� ??�?�: ?���?�s�?��?��?�R�?�T?��?���?�9�@ 9�@֌@sm@MAš�Aũ�Aũ�AŲ-AŲ-Aũ�Aũ�Ař�A�O�A��/A�r�A�9XAöFA��-A���A�dZA��9A�/A�;dA�M�A�bNA�A�A�r�A��;A�7LA��A�;dA�v�A�jA�\)A��A���A� �A��/Ao�
A]�AN�+A>�HA1�^A&��AC�AdZA��@�5?@�Q�@�Ĝ@���@���@��@�\)@���@~��@r�!@i�^@_|�@Y�11111111111111111111111111111111111111111111111111111111Aš�Aũ�Aũ�AŲ-AŲ-Aũ�Aũ�Ař�A�O�A��/A�r�A�9XAöFA��-A���A�dZA��9A�/A�;dA�M�A�bNA�A�A�r�A��;A�7LA��A�;dA�v�A�jA�\)A��A���A� �A��/Ao�
A]�AN�+A>�HA1�^A&��AC�AdZA��@�5?@�Q�@�Ĝ@���@���@��@�\)@���@~��@r�!@i�^@_|�@Y�11111111111111111111111111111111111111111111111111111111;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBA�BC�BC�BC�BM�BXBdZBp�B�DB��B�RB�^B�wBB�jB�jB��BĜB��B�B�/B�yB�yB�/B��B�RB��B�\B�Bv�BM�B�BXB
��B
5?B	�B	hsB	&�B��B��B�B��Bz�Br�BjBn�B�BȴB	B	;dB	l�B	��B	ƨB	�mB	��B
111111111111111111111111111111111111111111111111111111111B/�B1�B1�B1�B<PBFtBR�B_�Bz^B��B�B��B�UB��B��B�B��B��B�B�)BϫB�=B��B��B�cB�B��B�Bs�Bn�BAoB�8BJ�B
��B
(�B	��B	[qB	eB��B�_B�dB�	Bl�Be,B\B_�Bs�B��B�B	+QB	\)B	�PB	�B	��B	�B	�211111111111111111111111111111111111111111111111111111111<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            PSAL_ADJ corrects Cnd. Thermal Mass (CTM), Johnson et al., 2007, JAOT; PSAL_ADJ = CTM_ADJ_PSAL + dS, dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment term r.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           CTM: alpha=0.141C, tau=6.89s with error equal to the adjustment; OW V1.0: r =0.9996(+/-0), vertically averaged dS =-0.0169(+/-0.0006)                                                                                                                           SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Significant salinity drift present; OW weighted least squares fit is adopted; Map Scales:[x:2/1,y:1/0.5]; max_breaks=0;                                                                                PSAL_ADJ_ERR: max(0.01, OW + CTM + resolution error)     202008191756132020081917561320200819175613  AO  ARCAADJS                                                                    20181121052846    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181121052846  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181121052846  QCF$                G�O�G�O�G�O�0               SI  ARCASIQCV2.1                                                                20200819175704  IP                  G�O�G�O�G�O�                SI  ARSQOW  V1.0ARGO_for_DMQC Climatology Version 2020V01                       20200819175704  IP                  G�O�G�O�G�O�                SI  ARDU                                                                        20200819175704  IP                  G�O�G�O�G�O�                