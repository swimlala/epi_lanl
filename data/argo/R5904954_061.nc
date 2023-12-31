CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:17:02Z creation      
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
resolution        =���   axis      Z        �  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  A8   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C,   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  T�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \x   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ^l   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  f4   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  h(   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  o�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  w�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  y�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �t   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �h   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �0   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �`   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �`   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �`   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �`   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �    HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20181005191702  20181005191702  5904954 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               =A   AO  6557                            2B  A   APEX                            7468                            062512                          846 @׾��|w�1   @׾�X�@5`ě��T�c��z�H1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      =A   A   A   @�33@�  A   A   A@  A`  A���A���A�33A�33A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BO��BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B���B�  B�  B�  B�  B���C  C  C  C  C
  C  C  C  C  C  C�fC  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C3�fC6  C8�C9�fC<  C>�C@  CB�CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX�CZ�C\  C^  C`  Ca�fCc�fCe�fCh  Ci�fCl  Cn  Cp  Cr  Ct�Cv  Cx  Cz  C|  C~  C�  C��C�  C��3C��3C��3C�  C��C�  C�  C��3C��3C��3C��3C��3C��3C��3C��3C��3C��C�  C�  C�  C��C��3C��C��C�  C��C��3C�  C�  C�  C�  C�  C��3C�  C�  C��C��C�  C��C�  C�  C��3C�  C�  C�  C�  C�  C��C��3C��fC��3C��3C��3C�  C��3C�  C��3C��C��C��C��C��C�  C��3C��3C��3C��C�  C��C�  C�  C��C�  C�  C�  C��3C��3C�  C�  C��C��C�  C�  C��C��3C�  C��C��C�  C�  C��C�  C�  C��C�  C��3C�  C�  C�  C��C��3C��3C�  C�  C�  C�  C��C�  C��3C��3C��3C��3C��3C��C��C��C��C��C�  C��C��C��C�  C�  C��3D   D � D ��D� DfD� D��Dy�D��D� DfD� D��Dy�D  D�fD  D� D	  D	� D	��D
� D  Dy�D��D� D  D� D��D� DfD� D��D� DfD� D  D�fDfD� D  Dy�D��D� DfD�fD��D� DfD�fDfD� DfD� D��Dy�D��Dy�D  Dy�D��Dy�DfD�fD fD � D ��D!� D"fD"� D#  D#�fD$fD$�fD%fD%� D%��D&� D'  D'�fD(  D(� D(��D)y�D*  D*�fD+  D+y�D,  D,y�D,��D-y�D.fD.� D/  D/��D0  D0� D1  D1� D2  D2� D3fD3� D4  D4�fD5  D5y�D6  D6�fD7fD7� D7��D8y�D8��D9�fD:fD:y�D;  D;�fD<fD<� D=  D=� D=��D>y�D>��D?y�D@  D@�fDA  DAy�DB  DB� DB��DC� DD  DDy�DE  DE� DE��DF� DGfDG�fDHfDH� DH��DIy�DI��DJy�DJ��DK� DLfDL�fDM  DM�fDNfDN�fDO  DO� DP  DP� DQ  DQy�DQ��DR� DS  DS� DTfDT�fDU  DUy�DV  DV�fDWfDWy�DW��DX� DX��DY� DZ  DZy�D[  D[y�D[��D\� D\��D]� D^  D^y�D^��D_� D`fD`� Da  Da�fDb  Db� Dc  Dc� DdfDd�fDe  De� De��Dfy�Dg  Dg�fDhfDh�fDifDi� Di��Djs3Dj��Dk� Dl  Dl�fDm  Dmy�Dm��Dn� Dn��Doy�Dp  Dp� Dq  Dqy�Dr  Dry�Ds  Ds� Dt  Dt� Du  Du� Dv  Dvy�Dw  Dw�fDw�fDyy�D�J=D�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�@ʏ\AG�A%G�AEG�AeG�A�p�A�p�A��
A��
A£�Aң�A��A��BQ�B	Q�BQ�BQ�B!Q�B)Q�B1Q�B9Q�BAQ�BIQ�BP�BYQ�BaQ�BiQ�BqQ�ByQ�B���B���B���B���B���B���B���B���B���B���B���B���B���B��)B�u�B���B���BĨ�BȨ�B̨�BШ�BԨ�Bب�Bܨ�B�u�B��B��B�u�B��B���B���B���C :�CT{CT{CT{CT{C
T{CT{CT{CT{CT{CT{C:�CT{CT{CT{CT{C T{C"T{C$T{C&T{C(T{C*T{C,T{C.T{C0T{C2T{C4:�C6T{C8nC::�C<T{C>nC@T{CBnCDT{CFT{CHT{CJT{CLT{CNT{CPT{CRT{CTT{CVT{CXnCZnC\T{C^T{C`T{Cb:�Cd:�Cf:�ChT{Cj:�ClT{CnT{CpT{CrT{CtnCvT{CxT{CzT{C|T{C~T{C�*=C�7
C�*=C�pC�pC�pC�*=C�7
C�*=C�*=C�pC�pC�pC�pC�pC�pC�pC�pC�pC�7
C�*=C�*=C�*=C�7
C�pC�7
C�7
C�*=C�7
C�pC�*=C�*=C�*=C�*=C�*=C�pC�*=C�*=C�7
C�7
C�*=C�7
C�*=C�*=C�pC�*=C�*=C�*=C�*=C�*=C�7
C�pC��C�pC�pC�pC�*=C�pC�*=C�pC�7
C�7
C�7
C�7
C�7
C�*=C�pC�pC�pC�7
C�*=C�7
C�*=C�*=C�7
C�*=C�*=C�*=C�pC�pC�*=C�*=C�7
C�7
C�*=C�*=C�7
C�pC�*=C�7
C�7
C�*=C�*=C�C�C�*=C�*=C�7
C�*=C�pC�*=C�*=C�*=C�7
C�pC�pC�*=C�*=C�*=C�*=C�7
C�*=C�pC�pC�pC�pC�pC�7
C�7
C�7
C�7
C�7
C�*=C�7
C�7
C�7
C�*=C�*=C�pD D �D�D�D�D�D�D��D�D�D�D�D�D��DD��DD�D	D	�D
�D
�DD��D�D�DD�D�D�D�D�D�D�D�D�DD��D�D�DD��D�D�D�D��D�D�D�D��D�D�D�D�D�D��D�D��DD��D�D��D�D��D �D �D!�D!�D"�D"�D#D#��D$�D$��D%�D%�D&�D&�D'D'��D(D(�D)�D)��D*D*��D+D+��D,D,��D-�D-��D.�D.�D/D/��D0D0�D1D1�D2D2�D3�D3�D4D4��D5D5��D6D6��D7�D7�D8�D8��D9�D9��D:�D:��D;D;��D<�D<�D=D=�D>�D>��D?�D?��D@D@��DADA��DBDB�DC�DC�DDDD��DEDE�DF�DF�DG�DG��DH�DH�DI�DI��DJ�DJ��DK�DK�DL�DL��DMDM��DN�DN��DODO�DPDP�DQDQ��DR�DR�DSDS�DT�DT��DUDU��DVDV��DW�DW��DX�DX�DY�DY�DZDZ��D[D[��D\�D\�D]�D]�D^D^��D_�D_�D`�D`�DaDa��DbDb�DcDc�Dd�Dd��DeDe�Df�Df��DgDg��Dh�Dh��Di�Di�Dj�Dj�RDk�Dk�DlDl��DmDm��Dn�Dn�Do�Do��DpDp�DqDq��DrDr��DsDs�DtDt�DuDu�DvDv��DwDw��Dw��Dy��D�T�D��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�ZA�\)A�^5A�^5A�Q�A�VA�O�A�33A�hsA�`BA�|�Aϩ�A�1Aΰ!AΟ�A�v�ÃA�oA�~�A�/A�1A���A�Q�A��Aǝ�A�C�A�JA��mAÙ�A�\)A��A�%A��A��
APA�"�A���A���A���A���A�A�A�{A���A�hsA��hA�JA�z�A�~�A�(�A���A�VA��A��
A�M�A��DA�VA��A���A�|�A���A�33A��wA�
=A�Q�A��A�JA���A��hA��A���A��A��A��TA��-A�
=A�+A���A��A��A��/A�;dA�JA�-A�VA�ȴA�XA�ĜA���A��jA��!A�?}A��;A�+A�A���A��A���A��A��A���A�Q�A���A�n�Ax�A}�mAy�;At�Arv�Ap�yAn��Am�Ak�Aj�DAjM�Ai��Ah��AhM�Ag`BAe�-Ac��Ab-A_33A]
=AY�wAVbNAS+ARr�AQ�AP�AP1'AO�AN�AN^5AMO�AK��AJ  AI"�AE33AB��AA�mA@$�A=K�A:(�A7�
A6$�A1��A/hsA. �A-33A+�A)�A'�A$�DA"VA!A z�AA�PAVA�`Ap�A�AA�^A�AJAA�A"�A��An�Ax�AO�A�`AM�AdZA"�A�A
ȴA
bNA
{A	p�A$�A�A��A|�AVA�A��AM�AAoA��A`BA n�@�~�@�r�@�I�@��m@���@�\)@�C�@�;d@��@��@��\@�{@���@�`B@�/@���@��H@���@���@���@�$�@��y@��@�7@��@���@홚@�X@��/@�j@�C�@�\@�@�7L@�Z@�o@�v�@�$�@���@��@�l�@�@��@�C�@�~�@�{@݉7@ܴ9@��@ۥ�@���@�{@ج@�A�@�1'@׮@�^5@�p�@�t�@��@�ȴ@���@Ұ!@ҧ�@җ�@�V@�X@�ƨ@�~�@�@Ͳ-@�G�@�Z@�;d@ʰ!@ʇ+@ɲ-@�I�@�+@Ƈ+@�J@�p�@ļj@�1@��m@���@�ƨ@Å@�S�@��@¸R@�{@���@��@��D@���@��+@�@��@��-@�7L@��9@�bN@�Z@�ƨ@��\@��-@�G�@�Ĝ@��j@��D@�Q�@�b@��@�C�@��@�ff@�{@��#@��7@�x�@�X@��@���@��/@�A�@��m@��@��@���@�^5@�=q@�J@��#@��-@���@�hs@�G�@�7L@��@�Ĝ@�1'@�b@��@��
@���@�ƨ@���@�K�@���@���@�5?@���@�%@���@��@�&�@��@���@�Ĝ@�b@�K�@���@�~�@�M�@�{@�@�hs@�`B@�O�@�V@��D@��@���@�S�@���@�ff@�5?@�?}@��u@��
@���@��@��@��P@��@�K�@���@���@�n�@�E�@���@�&�@�V@��9@�  @��@��P@���@���@��@���@�@��^@��^@�@�@�@�@�@�%@��
@��;@�|�@�;d@�
=@��y@��H@�~�@�J@���@��h@�/@��@��/@��`@��`@��`@��j@�Q�@�ƨ@��@�l�@���@���@�v�@�E�@�@�hs@�/@��@���@���@�Ĝ@��9@�j@�b@�ƨ@��F@���@�t�@�\)@�33@�~�@�ff@�ff@�V@�5?@�{@���@��@��@��@� �@���@�|�@�S�@��@���@���@�-@���@��7@��7@�p�@�hs@�X@��@��/@��/@��j@�z�@�A�@�  @�ƨ@��w@��F@��F@��@���@�"�@��R@�~�@�V@���@�x�@�`B@�7L@��@���@�Ĝ@��@v�b@a�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�ZA�\)A�^5A�^5A�Q�A�VA�O�A�33A�hsA�`BA�|�Aϩ�A�1Aΰ!AΟ�A�v�ÃA�oA�~�A�/A�1A���A�Q�A��Aǝ�A�C�A�JA��mAÙ�A�\)A��A�%A��A��
APA�"�A���A���A���A���A�A�A�{A���A�hsA��hA�JA�z�A�~�A�(�A���A�VA��A��
A�M�A��DA�VA��A���A�|�A���A�33A��wA�
=A�Q�A��A�JA���A��hA��A���A��A��A��TA��-A�
=A�+A���A��A��A��/A�;dA�JA�-A�VA�ȴA�XA�ĜA���A��jA��!A�?}A��;A�+A�A���A��A���A��A��A���A�Q�A���A�n�Ax�A}�mAy�;At�Arv�Ap�yAn��Am�Ak�Aj�DAjM�Ai��Ah��AhM�Ag`BAe�-Ac��Ab-A_33A]
=AY�wAVbNAS+ARr�AQ�AP�AP1'AO�AN�AN^5AMO�AK��AJ  AI"�AE33AB��AA�mA@$�A=K�A:(�A7�
A6$�A1��A/hsA. �A-33A+�A)�A'�A$�DA"VA!A z�AA�PAVA�`Ap�A�AA�^A�AJAA�A"�A��An�Ax�AO�A�`AM�AdZA"�A�A
ȴA
bNA
{A	p�A$�A�A��A|�AVA�A��AM�AAoA��A`BA n�@�~�@�r�@�I�@��m@���@�\)@�C�@�;d@��@��@��\@�{@���@�`B@�/@���@��H@���@���@���@�$�@��y@��@�7@��@���@홚@�X@��/@�j@�C�@�\@�@�7L@�Z@�o@�v�@�$�@���@��@�l�@�@��@�C�@�~�@�{@݉7@ܴ9@��@ۥ�@���@�{@ج@�A�@�1'@׮@�^5@�p�@�t�@��@�ȴ@���@Ұ!@ҧ�@җ�@�V@�X@�ƨ@�~�@�@Ͳ-@�G�@�Z@�;d@ʰ!@ʇ+@ɲ-@�I�@�+@Ƈ+@�J@�p�@ļj@�1@��m@���@�ƨ@Å@�S�@��@¸R@�{@���@��@��D@���@��+@�@��@��-@�7L@��9@�bN@�Z@�ƨ@��\@��-@�G�@�Ĝ@��j@��D@�Q�@�b@��@�C�@��@�ff@�{@��#@��7@�x�@�X@��@���@��/@�A�@��m@��@��@���@�^5@�=q@�J@��#@��-@���@�hs@�G�@�7L@��@�Ĝ@�1'@�b@��@��
@���@�ƨ@���@�K�@���@���@�5?@���@�%@���@��@�&�@��@���@�Ĝ@�b@�K�@���@�~�@�M�@�{@�@�hs@�`B@�O�@�V@��D@��@���@�S�@���@�ff@�5?@�?}@��u@��
@���@��@��@��P@��@�K�@���@���@�n�@�E�@���@�&�@�V@��9@�  @��@��P@���@���@��@���@�@��^@��^@�@�@�@�@�@�%@��
@��;@�|�@�;d@�
=@��y@��H@�~�@�J@���@��h@�/@��@��/@��`@��`@��`@��j@�Q�@�ƨ@��@�l�@���@���@�v�@�E�@�@�hs@�/@��@���@���@�Ĝ@��9@�j@�b@�ƨ@��F@���@�t�@�\)@�33@�~�@�ff@�ff@�V@�5?@�{@���@��@��@��@� �@���@�|�@�S�@��@���@���@�-@���@��7@��7@�p�@�hs@�X@��@��/@��/@��j@�z�@�A�@�  @�ƨ@��w@��F@��F@��@���@�"�@��R@�~�@�V@���@�x�@�`B@�7L@��@���@�Ĝ@��@v�b@a�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B��B��B��B��B��B��B��B�{B�=B��B�}BȴB��B��B�B�B�sB�B�B��B��B��B��B	7BoB9XB:^B<jBB�BH�BO�BP�BR�BS�BXBhsBt�B{�B�B��B��B�'B�B�B�B�9B�3B�'B�B�B��B��B��B��B��B�{B�VB�DB�7B�B�B|�Bw�Bp�BYBXBbNB\)B^5B^5BN�B8RB#�B��B�qB��B��B�7Bs�BbNBYB`BBC�BoBVBB
��B
�B
�B
ɺB
ÖB
ȴB
ĜB
��B
��B
�wB
�RB
�B
��B
�oB
�JB
�B
�B
t�B
iyB
M�B
#�B
hB
B	�B	�BB	��B	��B	ȴB	ŢB	�wB	�XB	�'B	��B	�oB	�B	l�B	_;B	M�B	:^B	+B	%�B	!�B	�B	�B	�B	uB	\B		7B	B��B�B�B�NB�/B�
B��B��B��BŢBĜB�wB�XB�FB�3B�B��B�{B�=B�7B�+B�B|�Bz�Bw�Bv�Bt�Br�Bq�Bn�Bm�Bk�BffBbNB^5B]/B`BBaHBaHBbNBdZBffBe`BbNB`BB`BB\)BZB[#B_;Bm�Bq�Bs�Bt�Bs�B}�B�JB�PB�JB�DB�1B�1B�1B�+B�+B�+B�+B�+B�+B�1B�7B�7B�=B�=B�uB��B��B��B��B��B��B��B��B�B�B�B�B�B�B�B�B�B�B�B�!B�!B�!B�-B�-B�3B�9B�9B�FB�FB�LB�LB�RB�XB�XB�^B�dB�qB�wB�}BBƨBɺB��B��B��B��B��B��B��B��B��B�B�B�B�#B�)B�;B�ZB�mB�sB�B�B�B��B��B��B��B	  B	B	B	B	B	B	B	B	1B	1B		7B	JB	bB	{B	�B	�B	�B	�B	�B	�B	�B	�B	$�B	'�B	(�B	+B	+B	,B	,B	-B	/B	2-B	49B	6FB	8RB	9XB	;dB	<jB	<jB	>wB	?}B	?}B	C�B	F�B	J�B	N�B	Q�B	T�B	VB	XB	ZB	[#B	\)B	]/B	_;B	`BB	aHB	bNB	e`B	ffB	gmB	iyB	l�B	o�B	q�B	u�B	z�B	{�B	|�B	�B	�%B	�=B	�JB	�VB	�\B	�bB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	�B	�B	�B	�-B	�FB	�XB	�^B	�dB	�jB	�wB	�}B	�}B	��B	�}B	�}B	�}B	��B	ÖB	ĜB	ŢB	ƨB	ǮB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�#B	�)B	�/B	�)B	�)B	�)B	�)B	�/B	�5B	�;B	�HB	�HB	�HB	�NB	�NB	�NB	�NB	�TB	�`B	�fB	�fB	�fB	�mB	�mB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
�B
�B
/�222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  B��B��B��B��B��B��B��B�{B�=B��B�}BȴB��B��B�B�B�sB�B�B��B��B��B��B	7BoB9XB:^B<jBB�BH�BO�BP�BR�BS�BXBhsBt�B{�B�B��B��B�'B�B�B�B�9B�3B�'B�B�B��B��B��B��B��B�{B�VB�DB�7B�B�B|�Bw�Bp�BYBXBbNB\)B^5B^5BN�B8RB#�B��B�qB��B��B�7Bs�BbNBYB`BBC�BoBVBB
��B
�B
�B
ɺB
ÖB
ȴB
ĜB
��B
��B
�wB
�RB
�B
��B
�oB
�JB
�B
�B
t�B
iyB
M�B
#�B
hB
B	�B	�BB	��B	��B	ȴB	ŢB	�wB	�XB	�'B	��B	�oB	�B	l�B	_;B	M�B	:^B	+B	%�B	!�B	�B	�B	�B	uB	\B		7B	B��B�B�B�NB�/B�
B��B��B��BŢBĜB�wB�XB�FB�3B�B��B�{B�=B�7B�+B�B|�Bz�Bw�Bv�Bt�Br�Bq�Bn�Bm�Bk�BffBbNB^5B]/B`BBaHBaHBbNBdZBffBe`BbNB`BB`BB\)BZB[#B_;Bm�Bq�Bs�Bt�Bs�B}�B�JB�PB�JB�DB�1B�1B�1B�+B�+B�+B�+B�+B�+B�1B�7B�7B�=B�=B�uB��B��B��B��B��B��B��B��B�B�B�B�B�B�B�B�B�B�B�B�!B�!B�!B�-B�-B�3B�9B�9B�FB�FB�LB�LB�RB�XB�XB�^B�dB�qB�wB�}BBƨBɺB��B��B��B��B��B��B��B��B��B�B�B�B�#B�)B�;B�ZB�mB�sB�B�B�B��B��B��B��B	  B	B	B	B	B	B	B	B	1B	1B		7B	JB	bB	{B	�B	�B	�B	�B	�B	�B	�B	�B	$�B	'�B	(�B	+B	+B	,B	,B	-B	/B	2-B	49B	6FB	8RB	9XB	;dB	<jB	<jB	>wB	?}B	?}B	C�B	F�B	J�B	N�B	Q�B	T�B	VB	XB	ZB	[#B	\)B	]/B	_;B	`BB	aHB	bNB	e`B	ffB	gmB	iyB	l�B	o�B	q�B	u�B	z�B	{�B	|�B	�B	�%B	�=B	�JB	�VB	�\B	�bB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	�B	�B	�B	�-B	�FB	�XB	�^B	�dB	�jB	�wB	�}B	�}B	��B	�}B	�}B	�}B	��B	ÖB	ĜB	ŢB	ƨB	ǮB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�#B	�)B	�/B	�)B	�)B	�)B	�)B	�/B	�5B	�;B	�HB	�HB	�HB	�NB	�NB	�NB	�NB	�TB	�`B	�fB	�fB	�fB	�mB	�mB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
�B
�B
/�222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.33 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005191702                              AO  ARCAADJP                                                                    20181005191702    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005191702  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005191702  QCF$                G�O�G�O�G�O�8000            