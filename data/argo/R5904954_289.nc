CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:17:55Z creation      
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20181005191755  20181005191755  5904954 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL              !A   AO  6557                            2B  A   APEX                            7468                            062512                          846 @��� ��1   @����%��@5R���m�d
=p��1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                     !A   A   A   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A���A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�33B�  B���B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C�C�C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB�CD  CF  CH  CJ  CL�CN�CP�CR  CT  CV�CX  CY�fC\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl�Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C}�fC�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C��C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C��C�  C��3C��3C�  C��C�  C��3C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C��C�  C��3C�  C��C�  C��3C��3C��3C��C��C��C�  C�  C�  C��3C��3C�  C�  C�  C�  C��C��C�  C�  C��3C��3C��3C��3C�  C��C�  C�  C��C��C��C�  C��3C�  C��3C��3C��3C�  C��C�  C��3C�  C�  C��3C�  C��C��C��3C��fC��3C�  C�  C�  C��3C��3C��C�  D   D �fDfD�fD  D� D  D�fDfD� D��D� D  D� D  D� D  D� D	  D	�fD
  D
� D
��Dy�D  D��D  D� D  Dy�D  D� D  Dy�D  D� D  D�fD  D� D��Ds3D  D��DfD� D��Dy�D��Dy�D  D� D��D� D  D� D  D�fD  Dy�D  Dy�D��Dy�D   D � D ��D!s3D!��D"� D#fD#� D$  D$� D%fD%�fD&  D&� D'  D'� D'��D(� D)  D)� D*fD*�fD+fD+�fD,fD,�fD-fD-�fD.  D.� D/  D/y�D/��D0�fD1fD1�fD1��D2y�D2��D3� D4  D4� D5fD5� D5��D6� D7  D7y�D8  D8� D9  D9y�D:fD:� D:��D;y�D;��D<y�D=fD=s3D>  D>�fD?  D?� D?��D@� DAfDA� DB  DB�fDCfDC� DD  DD�fDEfDE� DF  DF� DGfDG� DH  DH� DI  DI� DI��DJy�DJ��DK� DL  DL�fDMfDM� DN  DN� DO  DO�fDP  DPy�DP��DQ� DRfDR� DR��DS� DTfDT�fDUfDU�fDVfDV� DV��DW� DX  DXy�DY  DY�fDZfDZ� D[  D[� D[��D\y�D\��D]y�D]��D^�fD_  D_� D`fD`�fDafDa� Db  Db� Dc  Dc� Dd  Dd� Dd��De� DffDf� Df��Dgy�Dh  Dh�fDh��Diy�Di��Djy�Dj��Dk� DlfDl� Dm  Dm� Dn  Dn� Do  Do� Do��Dpy�Dq  Dq� DrfDr� Dr��Ds� DtfDty�Dt��Duy�Du�3Dvy�Dv��Dwy�Dw�3Dy�qD�:=D��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�@ʏ\AG�A%G�AEG�AeG�A���A���A���A���A£�A�p�A��A��BQ�B	Q�BQ�BQ�B!Q�B)Q�B1Q�B9Q�BAQ�BIQ�BQQ�BYQ�BaQ�BiQ�BqQ�ByQ�B���B���B���B���B���B���B���B���B���B���B���B���B���B��)B���B���B���BĨ�BȨ�B̨�B��)BԨ�Bب�Bܨ�B��B��)B��B�u�B��B���B���B���C T{CT{CT{CT{CT{C
T{CT{CT{CT{CT{CT{CT{CT{CnCnCT{C T{C"T{C$T{C&T{C(T{C*T{C,T{C.T{C0T{C2T{C4T{C6T{C8T{C:T{C<T{C>T{C@T{CBnCDT{CFT{CHT{CJT{CLnCNnCPnCRT{CTT{CVnCXT{CZ:�C\T{C^T{C`T{CbT{CdT{CfT{ChT{CjT{ClnCnT{CpT{CrT{CtT{CvT{CxT{CzT{C|T{C~:�C�*=C�*=C�*=C�*=C�*=C�*=C�pC�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�pC�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�pC�*=C�*=C�*=C�7
C�*=C�*=C�*=C�*=C�*=C�7
C�7
C�*=C�*=C�*=C�*=C�7
C�*=C�pC�pC�*=C�7
C�*=C�pC�*=C�*=C�*=C�*=C�pC�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�pC�pC�*=C�7
C�*=C�pC�*=C�7
C�*=C�pC�pC�pC�7
C�7
C�7
C�*=C�*=C�*=C�pC�pC�*=C�*=C�*=C�*=C�7
C�7
C�*=C�*=C�pC�pC�pC�pC�*=C�7
C�*=C�*=C�7
C�7
C�7
C�*=C�pC�*=C�pC�pC�pC�*=C�7
C�*=C�pC�*=C�*=C�pC�*=C�7
C�7
C�pC��C�pC�*=C�*=C�*=C�pC�pC�7
C�*=D D ��D�D��DD�DD��D�D�D�D�DD�DD�DD�D	D	��D
D
�D�D��DD��DD�DD��DD�DD��DD�DD��DD�D�D�RDD��D�D�D�D��D�D��DD�D�D�DD�DD��DD��DD��D�D��D D �D!�D!�RD"�D"�D#�D#�D$D$�D%�D%��D&D&�D'D'�D(�D(�D)D)�D*�D*��D+�D+��D,�D,��D-�D-��D.D.�D/D/��D0�D0��D1�D1��D2�D2��D3�D3�D4D4�D5�D5�D6�D6�D7D7��D8D8�D9D9��D:�D:�D;�D;��D<�D<��D=�D=�RD>D>��D?D?�D@�D@�DA�DA�DBDB��DC�DC�DDDD��DE�DE�DFDF�DG�DG�DHDH�DIDI�DJ�DJ��DK�DK�DLDL��DM�DM�DNDN�DODO��DPDP��DQ�DQ�DR�DR�DS�DS�DT�DT��DU�DU��DV�DV�DW�DW�DXDX��DYDY��DZ�DZ�D[D[�D\�D\��D]�D]��D^�D^��D_D_�D`�D`��Da�Da�DbDb�DcDc�DdDd�De�De�Df�Df�Dg�Dg��DhDh��Di�Di��Dj�Dj��Dk�Dk�Dl�Dl�DmDm�DnDn�DoDo�Dp�Dp��DqDq�Dr�Dr�Ds�Ds�Dt�Dt��Du�Du��DvRDv��Dw�Dw��Dw�RDy��D�D�D�ȣ111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��A���A��A�&�A�JA��HA��A��A�K�A��A���A��
A��^A�|�A�M�A�/A��A�
=A���A��FA��A��A��uA���A���A���A�t�A�9XA� �A��A��TA�A���A��!A���A��DA�z�A�\)A�O�A�C�A�?}A�5?A�"�A�oA�oA���A��wA���A�hsA�A�A�/A���A��`A���A�ȴA��RA���A�&�A��-A��A�M�A��`A�33A��TA���A�/A�A���A�%A���A�S�A�x�A�x�A��A�VA�bNA�jA�VA��A�ƨA�\)A�S�A���A���A�oA���A���A��^A��A��/A�ffA���A�z�A���A�~�A�A��DA��HA� �A���A�7LA��/A��A�"�A���A���A���A�G�A���A��A�33A�1'A�+A���A�A�{A|ĜAx$�Au�Ar��Ap�Am�-Al�AkXAh��Af��Ad��AdQ�Ad-Ac�wAb�!Abr�AaK�A_O�A]�;AY��AX�AXjAW\)AVz�AUVARȴAP��AO�AN��AM+AJffAGoAE��AE�AD�AB�ABA@ZA<�9A<v�A;��A:�A:�A9��A9\)A9+A8�HA8�DA7��A6�A5��A4�\A2��A1�A0�+A.�9A.{A-oA,ĜA,�A+x�A+|�A+K�A*��A)ƨA(�A'��A&�+A%|�A$bNA#�;A#G�A"�A!�A!�A��A��A�9A��A�!A�A
=A�!AZA{A��A��A�A�7AM�AdZAffA��A�\A�RAA�A�#AO�A
�A	x�A�Av�A�mA��A��AXA��AO�AA�A�7A �u@��@��R@�K�@�@�v�@�(�@�hs@���@��@�@�%@�z�@��m@�C�@�$�@�+@��@畁@�@�M�@���@�Z@���@�@�`B@�Ĝ@�(�@�"�@�G�@��@�K�@�5?@�hs@�9X@�{@�Q�@���@���@��y@�hs@д9@�Z@�t�@�n�@�V@�C�@��`@�@Ƈ+@�M�@�-@��@�1'@��@���@�-@��h@���@��H@�V@��-@�Ĝ@��D@�r�@�1'@��;@�@��@���@��@�hs@�X@���@��D@�Z@�|�@��H@���@�ȴ@���@��-@�p�@�O�@�G�@�7L@��j@�9X@�^5@��#@�hs@���@�b@�l�@�{@���@��j@���@��u@�bN@�A�@�ƨ@�"�@���@�-@��@��^@�O�@���@��j@���@���@�r�@�(�@���@��;@��F@��@�"�@���@��+@�~�@�n�@�^5@�@��h@�hs@�G�@��@���@�A�@��@��w@��F@��@�l�@�+@���@�=q@��#@���@��h@�`B@�/@�%@���@�r�@�A�@�(�@�b@��m@��F@���@��@��P@�|�@�l�@�l�@�C�@�o@���@���@�v�@�{@�{@���@��@�`B@���@�bN@��@�  @��m@��F@��F@���@�
=@�~�@��-@�x�@�X@�G�@�7L@�%@��u@�A�@���@�;d@��\@��#@��7@�O�@��@��@��`@���@���@�A�@�b@��
@�ƨ@���@�S�@�
=@��@���@��@��\@�=q@�@��@��^@���@��7@�/@���@���@��@�j@�9X@���@��F@�l�@�33@���@��H@���@�ff@�@���@�O�@�%@��@��9@��@���@�z�@�A�@�1@�ƨ@�|�@�dZ@�;d@�o@�
=@��@�v�@�E�@�-@��@�@��@���@��^@��7@�X@�7L@�V@���@��`@��@��D@�I�@�  @���@��P@�@��R@���@�3�@{O@iS&111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��A���A��A�&�A�JA��HA��A��A�K�A��A���A��
A��^A�|�A�M�A�/A��A�
=A���A��FA��A��A��uA���A���A���A�t�A�9XA� �A��A��TA�A���A��!A���A��DA�z�A�\)A�O�A�C�A�?}A�5?A�"�A�oA�oA���A��wA���A�hsA�A�A�/A���A��`A���A�ȴA��RA���A�&�A��-A��A�M�A��`A�33A��TA���A�/A�A���A�%A���A�S�A�x�A�x�A��A�VA�bNA�jA�VA��A�ƨA�\)A�S�A���A���A�oA���A���A��^A��A��/A�ffA���A�z�A���A�~�A�A��DA��HA� �A���A�7LA��/A��A�"�A���A���A���A�G�A���A��A�33A�1'A�+A���A�A�{A|ĜAx$�Au�Ar��Ap�Am�-Al�AkXAh��Af��Ad��AdQ�Ad-Ac�wAb�!Abr�AaK�A_O�A]�;AY��AX�AXjAW\)AVz�AUVARȴAP��AO�AN��AM+AJffAGoAE��AE�AD�AB�ABA@ZA<�9A<v�A;��A:�A:�A9��A9\)A9+A8�HA8�DA7��A6�A5��A4�\A2��A1�A0�+A.�9A.{A-oA,ĜA,�A+x�A+|�A+K�A*��A)ƨA(�A'��A&�+A%|�A$bNA#�;A#G�A"�A!�A!�A��A��A�9A��A�!A�A
=A�!AZA{A��A��A�A�7AM�AdZAffA��A�\A�RAA�A�#AO�A
�A	x�A�Av�A�mA��A��AXA��AO�AA�A�7A �u@��@��R@�K�@�@�v�@�(�@�hs@���@��@�@�%@�z�@��m@�C�@�$�@�+@��@畁@�@�M�@���@�Z@���@�@�`B@�Ĝ@�(�@�"�@�G�@��@�K�@�5?@�hs@�9X@�{@�Q�@���@���@��y@�hs@д9@�Z@�t�@�n�@�V@�C�@��`@�@Ƈ+@�M�@�-@��@�1'@��@���@�-@��h@���@��H@�V@��-@�Ĝ@��D@�r�@�1'@��;@�@��@���@��@�hs@�X@���@��D@�Z@�|�@��H@���@�ȴ@���@��-@�p�@�O�@�G�@�7L@��j@�9X@�^5@��#@�hs@���@�b@�l�@�{@���@��j@���@��u@�bN@�A�@�ƨ@�"�@���@�-@��@��^@�O�@���@��j@���@���@�r�@�(�@���@��;@��F@��@�"�@���@��+@�~�@�n�@�^5@�@��h@�hs@�G�@��@���@�A�@��@��w@��F@��@�l�@�+@���@�=q@��#@���@��h@�`B@�/@�%@���@�r�@�A�@�(�@�b@��m@��F@���@��@��P@�|�@�l�@�l�@�C�@�o@���@���@�v�@�{@�{@���@��@�`B@���@�bN@��@�  @��m@��F@��F@���@�
=@�~�@��-@�x�@�X@�G�@�7L@�%@��u@�A�@���@�;d@��\@��#@��7@�O�@��@��@��`@���@���@�A�@�b@��
@�ƨ@���@�S�@�
=@��@���@��@��\@�=q@�@��@��^@���@��7@�/@���@���@��@�j@�9X@���@��F@�l�@�33@���@��H@���@�ff@�@���@�O�@�%@��@��9@��@���@�z�@�A�@�1@�ƨ@�|�@�dZ@�;d@�o@�
=@��@�v�@�E�@�-@��@�@��@���@��^@��7@�X@�7L@�V@���@��`@��@��D@�I�@�  @���@��P@�@��R@���@�3�@{O@iS&111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B@�B;dB8RB8RB5?B7LB6FB6FB7LB:^B;dB;dB:^B:^B;dB:^B<jB?}B>wB<jB<jB:^BE�BM�BO�BO�BK�BH�BH�BP�Be`Bu�B�B�7B�=B�DB�DB�7B�7B�=B�DB�JB�JB�VB�uB��B�{B�oB�oB�oB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�uB�=Bu�BcTBO�BC�B>wB;dB1'B�BuBhBbB
=B  B��B�B�B��B�jB�3B�B��B��B��B��B�bB�DB�B~�Bu�Bl�BdZBR�BM�BH�BC�B;dB,B�BVB
�B
�5B
ɺB
��B
�\B
�+B
y�B
k�B
K�B
'�B
hB
B	��B	�B	��B	��B	�mB	�B	��B	ƨB	ŢB	��B	�RB	�?B	�B	��B	�JB	l�B	e`B	`BB	ZB	S�B	L�B	>wB	33B	-B	%�B	�B	PB	  B��B��B��B�B�B�`B�#B�HB�NB�5B�)B�5B�/B�)B�#B�B�
B��B��B��B��B��BǮB�}B�jB�RB�XB�RB�LB�^B�RB�9B�!B�B��B��B��B��B��B��B�uB�bB�PB�=B�B�B~�B|�Bz�By�Bx�Bw�Bu�Bs�Bo�BjBffBffBiyBhsBffBe`BffBffBffBe`BdZBdZBjBm�Bo�Bq�Bm�Bl�Bn�BiyBgmBffBgmBiyBq�B{�B{�Bz�By�Bx�Bs�Bo�Bm�Bl�Bm�Bl�Bk�BhsBe`BdZBffBe`Be`Bl�Bo�Bs�B~�B�B�B�B�B�B�B�B�B�B�B�7B�PB�VB�\B�{B��B��B��B��B��B��B��B��B��B��B�B��B�B��B�B�B�!B�'B�-B�9B�?B�LB�jB�wBBĜBÖBÖBŢBȴB��B��B��B�B�BB�BB�NB�fB�sB�B�B��B��B��B��B��B	  B	B	PB	\B	oB	{B	{B	{B	�B	�B	�B	�B	!�B	"�B	$�B	(�B	+B	.B	0!B	33B	49B	7LB	:^B	=qB	>wB	@�B	A�B	C�B	E�B	F�B	H�B	J�B	L�B	O�B	Q�B	Q�B	R�B	S�B	W
B	YB	[#B	\)B	_;B	bNB	cTB	dZB	dZB	dZB	e`B	ffB	hsB	l�B	o�B	p�B	p�B	q�B	r�B	s�B	u�B	u�B	v�B	v�B	w�B	x�B	x�B	y�B	z�B	~�B	� B	�B	�B	�B	�+B	�1B	�JB	�bB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�'B	�3B	�?B	�^B	�qB	�}B	��B	B	B	ÖB	ĜB	ƨB	ƨB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�B	�B	�B	�#B	�)B	�5B	�;B	�BB	�BB	�HB	�NB	�TB	�TB	�ZB	�`B	�fB	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
%B
%B
+B
	7B

=B

=B
	7B

=B
DB
B
;B
./222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  B@�B;dB8RB8RB5?B7LB6FB6FB7LB:^B;dB;dB:^B:^B;dB:^B<jB?}B>wB<jB<jB:^BE�BM�BO�BO�BK�BH�BH�BP�Be`Bu�B�B�7B�=B�DB�DB�7B�7B�=B�DB�JB�JB�VB�uB��B�{B�oB�oB�oB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�uB�=Bu�BcTBO�BC�B>wB;dB1'B�BuBhBbB
=B  B��B�B�B��B�jB�3B�B��B��B��B��B�bB�DB�B~�Bu�Bl�BdZBR�BM�BH�BC�B;dB,B�BVB
�B
�5B
ɺB
��B
�\B
�+B
y�B
k�B
K�B
'�B
hB
B	��B	�B	��B	��B	�mB	�B	��B	ƨB	ŢB	��B	�RB	�?B	�B	��B	�JB	l�B	e`B	`BB	ZB	S�B	L�B	>wB	33B	-B	%�B	�B	PB	  B��B��B��B�B�B�`B�#B�HB�NB�5B�)B�5B�/B�)B�#B�B�
B��B��B��B��B��BǮB�}B�jB�RB�XB�RB�LB�^B�RB�9B�!B�B��B��B��B��B��B��B�uB�bB�PB�=B�B�B~�B|�Bz�By�Bx�Bw�Bu�Bs�Bo�BjBffBffBiyBhsBffBe`BffBffBffBe`BdZBdZBjBm�Bo�Bq�Bm�Bl�Bn�BiyBgmBffBgmBiyBq�B{�B{�Bz�By�Bx�Bs�Bo�Bm�Bl�Bm�Bl�Bk�BhsBe`BdZBffBe`Be`Bl�Bo�Bs�B~�B�B�B�B�B�B�B�B�B�B�B�7B�PB�VB�\B�{B��B��B��B��B��B��B��B��B��B��B�B��B�B��B�B�B�!B�'B�-B�9B�?B�LB�jB�wBBĜBÖBÖBŢBȴB��B��B��B�B�BB�BB�NB�fB�sB�B�B��B��B��B��B��B	  B	B	PB	\B	oB	{B	{B	{B	�B	�B	�B	�B	!�B	"�B	$�B	(�B	+B	.B	0!B	33B	49B	7LB	:^B	=qB	>wB	@�B	A�B	C�B	E�B	F�B	H�B	J�B	L�B	O�B	Q�B	Q�B	R�B	S�B	W
B	YB	[#B	\)B	_;B	bNB	cTB	dZB	dZB	dZB	e`B	ffB	hsB	l�B	o�B	p�B	p�B	q�B	r�B	s�B	u�B	u�B	v�B	v�B	w�B	x�B	x�B	y�B	z�B	~�B	� B	�B	�B	�B	�+B	�1B	�JB	�bB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�'B	�3B	�?B	�^B	�qB	�}B	��B	B	B	ÖB	ĜB	ƨB	ƨB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�B	�B	�B	�#B	�)B	�5B	�;B	�BB	�BB	�HB	�NB	�TB	�TB	�ZB	�`B	�fB	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
%B
%B
+B
	7B

=B

=B
	7B

=B
DB
B
;B
./222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.33 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005191755                              AO  ARCAADJP                                                                    20181005191755    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005191755  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005191755  QCF$                G�O�G�O�G�O�8000            