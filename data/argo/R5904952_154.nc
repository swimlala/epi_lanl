CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:05:39Z creation      
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
resolution        =���   axis      Z        l  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  @�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     l  B�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J$   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     l  L    TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  Sl   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  Z�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  \�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  d    TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  e�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  mh   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  t�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  v�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ~   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �d   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �    HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �$   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �4   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �8   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �<   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �@Argo profile    3.1 1.2 19500101000000  20181005190539  20181005190539  5904952 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6431                            2B  A   APEX                            7466                            062512                          846 @���{��1   @����o�@0�O�;dZ�c�I�^1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �A   A   B   @333@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A���A�  A�  B   B  B  B  B   B(  B0  B7��B@  BH  BP  BX  B`  Bg��Bo��Bx  B�  B�  B�  B�  B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  C   C  C  C  C  C
  C  C  C  C  C�C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB�CD  CF  CH  CJ�CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf�Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv�Cx  Cz  C{�fC}�fC�  C��C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C��C�  C��3C�  C��C��C��C��C�  C�  C��3C�  C�  C�  C�  C��3C�  C�  C�  C��C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C��C�  C�  C�  C��3C�  C�  C��3C�  C��C�  C�  C�  C�  D   D � DfD� D  D�fD  D� D  Dy�D��Dy�D��D� DfD� DfD�fD	fD	� D
  D
�fDfD� DfD�fD  D� D  D� D  Dy�D��D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fDfD� D  D� D  Dy�D  D� D  D� D��D� D  D� D fD � D ��D!� D"  D"� D#  D#y�D#��D$y�D$��D%y�D%��D&� D'fD'�fD(fD(� D)  D)y�D*  D*�fD+fD+�fD,  D,y�D-  D-� D.  D.� D/  D/y�D0  D0� D1  D1� D2  D2y�D3  D3� D4  D4�fD5fD5� D5��D6� D7  D7� D8  D8y�D9  D9� D:fD:� D;fD;� D;��D<y�D<��D=� D=��D>y�D>��D?� D@  D@y�DA  DA� DB  DB�fDC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DT  DTy�DU  DU� DV  DV� DW  DW�fDX  DX� DYfDY� DZ  DZ� D[  D[�fD\  D\� D]  D]� D^  D^� D_  D_� D`fD`� Da  Da� Db  Db� Dc  Dc� Dd  Dd�fDe  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dk��Dly�Dm  Dm� Dn  Dn�fDofDo� Do��Dpy�Dq  Dqy�Dq��Dry�Dr��Dsy�Ds��Dty�Du  Du� Du��Dv� Dw  Dwl�Dyc3D�J�D�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @HQ�@��\@ʏ\AG�A%G�AEG�AeG�A���A���A���A���A£�A�p�A��A��BQ�B	Q�BQ�BQ�B!Q�B)Q�B1Q�B8�BAQ�BIQ�BQQ�BYQ�BaQ�Bh�Bp�ByQ�B���B���B���B���B�u�B�u�B���B���B���B���B���B���B���B���B���B���B���BĨ�BȨ�B̨�BШ�B��)Bب�Bܨ�B��B��B��B��B��B��)B���B���C T{CT{CT{CT{CT{C
T{CT{CT{CT{CT{CnCT{CT{CT{CT{CT{C T{C"T{C$T{C&T{C(T{C*T{C,T{C.T{C0T{C2T{C4T{C6T{C8T{C:T{C<T{C>T{C@T{CBnCDT{CFT{CHT{CJnCLT{CNT{CPT{CRT{CTT{CVT{CXT{CZT{C\T{C^T{C`T{CbT{CdT{CfnChT{CjT{ClT{CnT{CpT{CrT{CtT{CvnCxT{CzT{C|:�C~:�C�*=C�7
C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�7
C�7
C�*=C�*=C�*=C�7
C�7
C�*=C�*=C�*=C�*=C�*=C�*=C�7
C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�7
C�7
C�7
C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�7
C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�pC�*=C�*=C�*=C�*=C�*=C�*=C�*=C�pC�pC�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�pC�*=C�7
C�*=C�pC�*=C�7
C�7
C�7
C�7
C�*=C�*=C�pC�*=C�*=C�*=C�*=C�pC�*=C�*=C�*=C�7
C�*=C�*=C�*=C�*=C�*=C�pC�pC�*=C�*=C�*=C�7
C�*=C�*=C�*=C�pC�*=C�*=C�pC�*=C�7
C�*=C�*=C�*=C�*=D D �D�D�DD��DD�DD��D�D��D�D�D�D�D�D��D	�D	�D
D
��D�D�D�D��DD�DD�DD��D�D�DD�DD�DD�DD�DD�DD�DD�DD��D�D�DD�DD��DD�DD�D�D�DD�D �D �D!�D!�D"D"�D#D#��D$�D$��D%�D%��D&�D&�D'�D'��D(�D(�D)D)��D*D*��D+�D+��D,D,��D-D-�D.D.�D/D/��D0D0�D1D1�D2D2��D3D3�D4D4��D5�D5�D6�D6�D7D7�D8D8��D9D9�D:�D:�D;�D;�D<�D<��D=�D=�D>�D>��D?�D?�D@D@��DADA�DBDB��DCDC�DDDD�DEDE�DFDF�DGDG�DHDTDT��DUDU�DVDV�DWDW��DXDX�DY�DY�DZDZ�D[D[��D\D\�D]D]�D^D^�D_D_�D`�D`�DaDa�DbDb�DcDc�DdDd��DeDe�DfDf�DgDg�DhDh�DiDi�DjDj�DkDk�Dl�Dl��DmDm�DnDn��Do�Do�Dp�Dp��DqDq��Dr�Dr��Ds�Ds��Dt�Dt��DuDu�Dv�Dv�DwDw��DyxRD�UpD��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�;dA�5?A�=qA�C�A�E�A�G�A�K�A�O�A�Q�A�VA�ZA�\)A�ffA�hsA�l�A�x�A͍PA͍PA͕�Aͥ�AͶFA�A;wA��TA���A�/A�`BA�bNA�ffA�jA�l�A�t�A�v�A΅AΓuAΛ�AΟ�AΡ�AΩ�AΩ�AΧ�AΥ�AΟ�AΙ�AΗ�AΕ�AΕ�AΕ�AΏ\A΍PA΋DA�~�A�x�A�p�A�`BA�A�A��A���A�n�A�jAɩ�A��`A�n�A�%A�(�A�r�A��TA�n�A��A�O�A��#A�p�A��A��7A�/A���A�ZA��hA�|�A���A�ZA��jA��FA���A�oA�ffA�1A�VA��+A��A��A�1'A�%A�p�A��`A�9XA�A�A���A���A��A�XA���A��TA�|�A��A��FA�A�A~1'Az�`Ax��AwAv{Ar  Ao7LAmAh�HAct�A_�FA^��A[C�AYAW�
AWK�AV��AVAT5?AR$�AO�FAN�AL�/AK�AJ�RAIƨAGx�ABĜAAoA?XA>(�A=oA97LA7�wA7XA7
=A5�A5/A3XA/�hA-�^A,JA+p�A++A*��A)�A($�A&��A&A%VA#�wA!��A��A�A9XAp�AVAA?}AJA+Ar�A�A��A �A�PA�HAVA��A��AoA\)A��A�^A%A��A�uA�A
�A	��A��AS�AM�AbA��A5?AK�A �A1At�A33Al�AS�AK�A �@��@���@�^5@���@�~�@��@���@��R@�M�@�@�@�n�@�{@���@�l�@��`@�t�@�ȴ@�$�@�v�@�Ĝ@�P@�\@�@�hs@��@�j@�bN@㝲@�"�@�~�@�-@��@�z�@߶F@�\)@�n�@��m@ٺ^@�?}@�X@؛�@���@��;@�K�@���@��#@�/@�z�@�\)@�{@�X@�1'@�\)@·+@�X@�Ĝ@�(�@�"�@�v�@�@���@ȼj@��`@�(�@�"�@Ƨ�@Ɨ�@���@��T@��@�V@���@ļj@�I�@�b@���@þw@�t�@��@+@�5?@��h@�?}@��@��@�&�@�%@��j@� �@��
@���@�K�@�
=@�v�@��@��h@�V@�9X@�+@�n�@�=q@��P@���@��@�x�@�&�@��@�"�@�ȴ@�ȴ@�o@��+@�5?@��T@��#@��^@�X@��@�A�@���@�K�@�+@�;d@�@���@�~�@�=q@���@���@�G�@�z�@�  @��m@�33@�M�@��@�x�@���@�j@��@�1@��m@��@���@�$�@���@��h@��7@�X@�&�@���@��D@��@��
@��P@�dZ@�@�v�@��@��@���@��-@�p�@�&�@��@���@��j@�I�@�bN@�r�@�r�@�  @��w@���@�|�@�;d@��@���@�@��-@��7@�hs@��@��@�V@���@��@�Z@�r�@��m@�
=@��R@���@�v�@�ff@�5?@��@��@�l�@��@���@�n�@�E�@�$�@��@�J@��^@��h@�`B@��@���@��@�9X@�1@��m@���@�;d@�@��y@���@�5?@�J@��T@���@�?}@��@���@��j@��u@�j@��@���@���@��@�l�@�+@�o@��H@���@�v�@�$�@�{@�J@�@���@�x�@��@���@���@�z�@�Z@�Z@�I�@�1'@� �@���@��@��@�K�@�C�@�;d@�"�@��@��!@�v�@�ff@�V@�E�@�=q@��>@|/�@e��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�;dA�5?A�=qA�C�A�E�A�G�A�K�A�O�A�Q�A�VA�ZA�\)A�ffA�hsA�l�A�x�A͍PA͍PA͕�Aͥ�AͶFA�A;wA��TA���A�/A�`BA�bNA�ffA�jA�l�A�t�A�v�A΅AΓuAΛ�AΟ�AΡ�AΩ�AΩ�AΧ�AΥ�AΟ�AΙ�AΗ�AΕ�AΕ�AΕ�AΏ\A΍PA΋DA�~�A�x�A�p�A�`BA�A�A��A���A�n�A�jAɩ�A��`A�n�A�%A�(�A�r�A��TA�n�A��A�O�A��#A�p�A��A��7A�/A���A�ZA��hA�|�A���A�ZA��jA��FA���A�oA�ffA�1A�VA��+A��A��A�1'A�%A�p�A��`A�9XA�A�A���A���A��A�XA���A��TA�|�A��A��FA�A�A~1'Az�`Ax��AwAv{Ar  Ao7LAmAh�HAct�A_�FA^��A[C�AYAW�
AWK�AV��AVAT5?AR$�AO�FAN�AL�/AK�AJ�RAIƨAGx�ABĜAAoA?XA>(�A=oA97LA7�wA7XA7
=A5�A5/A3XA/�hA-�^A,JA+p�A++A*��A)�A($�A&��A&A%VA#�wA!��A��A�A9XAp�AVAA?}AJA+Ar�A�A��A �A�PA�HAVA��A��AoA\)A��A�^A%A��A�uA�A
�A	��A��AS�AM�AbA��A5?AK�A �A1At�A33Al�AS�AK�A �@��@���@�^5@���@�~�@��@���@��R@�M�@�@�@�n�@�{@���@�l�@��`@�t�@�ȴ@�$�@�v�@�Ĝ@�P@�\@�@�hs@��@�j@�bN@㝲@�"�@�~�@�-@��@�z�@߶F@�\)@�n�@��m@ٺ^@�?}@�X@؛�@���@��;@�K�@���@��#@�/@�z�@�\)@�{@�X@�1'@�\)@·+@�X@�Ĝ@�(�@�"�@�v�@�@���@ȼj@��`@�(�@�"�@Ƨ�@Ɨ�@���@��T@��@�V@���@ļj@�I�@�b@���@þw@�t�@��@+@�5?@��h@�?}@��@��@�&�@�%@��j@� �@��
@���@�K�@�
=@�v�@��@��h@�V@�9X@�+@�n�@�=q@��P@���@��@�x�@�&�@��@�"�@�ȴ@�ȴ@�o@��+@�5?@��T@��#@��^@�X@��@�A�@���@�K�@�+@�;d@�@���@�~�@�=q@���@���@�G�@�z�@�  @��m@�33@�M�@��@�x�@���@�j@��@�1@��m@��@���@�$�@���@��h@��7@�X@�&�@���@��D@��@��
@��P@�dZ@�@�v�@��@��@���@��-@�p�@�&�@��@���@��j@�I�@�bN@�r�@�r�@�  @��w@���@�|�@�;d@��@���@�@��-@��7@�hs@��@��@�V@���@��@�Z@�r�@��m@�
=@��R@���@�v�@�ff@�5?@��@��@�l�@��@���@�n�@�E�@�$�@��@�J@��^@��h@�`B@��@���@��@�9X@�1@��m@���@�;d@�@��y@���@�5?@�J@��T@���@�?}@��@���@��j@��u@�j@��@���@���@��@�l�@�+@�o@��H@���@�v�@�$�@�{@�J@�@���@�x�@��@���@���@�z�@�Z@�Z@�I�@�1'@� �@���@��@��@�K�@�C�@�;d@�"�@��@��!@�v�@�ff@�V@�E�@�=q@��>@|/�@e��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�B�B�B�B�B�B�B�B�B �B!�B"�B&�B'�B(�B-B5?B5?B8RB<jBB�BE�BD�BO�BW
BhsBy�Bz�B~�B�1B�\B�PB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�dBÖB�B�B�B��BBB	7BPB"�B49B;dB@�BC�BT�BiyBx�B�B�+B|�B}�B�%B�B{�Bx�BbNBO�B?}B#�B��B�sB��BƨB�B��Bo�B=qB�BB
�B
�dB
��B
�B
iyB
XB
O�B
>wB
uB	��B	�RB	�B	��B	��B	�oB	�B	m�B	L�B	'�B	�B	�B	#�B	�B	�B	uB	bB	
=B	B��B�B�B�B�fB�HB�/B�
B��BƨBÖB��B�jB�dB�^B�RB�LB�?B�-B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�'B�!B�B�B�B�3B�?B�9B�FB�9B�?B�FB�^BĜB��B��B��B��B��B��B�B�NB�fB�`B�mB�yB�yB�sB�B�B�yB�ZB�BB�NB�NB�fB�B�mB�`B�`B�`B�`B�fB�fB�mB�B�B�B�B��B��B��B��B��B��B��B��B	B	B	B		7B	JB	PB	PB	\B	�B	{B	oB	uB	�B	�B	�B	�B	 �B	(�B	,B	0!B	33B	7LB	8RB	:^B	<jB	=qB	A�B	E�B	G�B	I�B	K�B	L�B	N�B	Q�B	VB	XB	YB	ZB	[#B	^5B	`BB	`BB	aHB	aHB	cTB	dZB	dZB	e`B	ffB	iyB	k�B	m�B	p�B	q�B	r�B	s�B	s�B	u�B	w�B	x�B	y�B	y�B	v�B	v�B	y�B	~�B	� B	�B	�B	�B	�7B	�JB	�PB	�PB	�VB	�\B	�hB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�-B	�3B	�3B	�FB	�XB	�RB	�RB	�RB	�XB	�dB	�jB	�jB	�jB	�jB	�qB	�qB	�wB	��B	B	ÖB	ÖB	ĜB	ŢB	ǮB	ȴB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�#B	�#B	�)B	�/B	�5B	�/B	�)B	�/B	�5B	�HB	�HB	�TB	�TB	�NB	�TB	�fB	�mB	�mB	�mB	�mB	�sB	�yB	�y@EB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
%B
%B
+B
+B
+B
1B
1B
	7B
	7B
	7B
1B
1B
1B
	7B
	7B

=B
DB
DB
DB
DB
DB
JB
JB
JB
PB
VB
VB
VB
VB
\B
\B
bB
bB
bB
hB
hB
�B
(>B
2�2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222224222222222222222222222222222222222222222222222222222222222222222222222222222 B�B�B�B�B�B�B�B�B�B �B!�B"�B&�B'�B(�B-B5?B5?B8RB<jBB�BE�BD�BO�BW
BhsBy�Bz�B~�B�1B�\B�PB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�dBÖB�B�B�B��BBB	7BPB"�B49B;dB@�BC�BT�BiyBx�B�B�+B|�B}�B�%B�B{�Bx�BbNBO�B?}B#�B��B�sB��BƨB�B��Bo�B=qB�BB
�B
�dB
��B
�B
iyB
XB
O�B
>wB
uB	��B	�RB	�B	��B	��B	�oB	�B	m�B	L�B	'�B	�B	�B	#�B	�B	�B	uB	bB	
=B	B��B�B�B�B�fB�HB�/B�
B��BƨBÖB��B�jB�dB�^B�RB�LB�?B�-B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�'B�!B�B�B�B�3B�?B�9B�FB�9B�?B�FB�^BĜB��B��B��B��B��B��B�B�NB�fB�`B�mB�yB�yB�sB�B�B�yB�ZB�BB�NB�NB�fB�B�mB�`B�`B�`B�`B�fB�fB�mB�B�B�B�B��B��B��B��B��B��B��B��B	B	B	B		7B	JB	PB	PB	\B	�B	{B	oB	uB	�B	�B	�B	�B	 �B	(�B	,B	0!B	33B	7LB	8RB	:^B	<jB	=qB	A�B	E�B	G�B	I�B	K�B	L�B	N�B	Q�B	VB	XB	YB	ZB	[#B	^5B	`BB	`BB	aHB	aHB	cTB	dZB	dZB	e`B	ffB	iyB	k�B	m�B	p�B	q�B	r�B	s�B	s�B	u�B	w�B	x�B	y�B	y�B	v�B	v�B	y�B	~�B	� B	�B	�B	�B	�7B	�JB	�PB	�PB	�VB	�\B	�hB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�-B	�3B	�3B	�FB	�XB	�RB	�RB	�RB	�XB	�dB	�jB	�jB	�jB	�jB	�qB	�qB	�wB	��B	B	ÖB	ÖB	ĜB	ŢB	ǮB	ȴB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�#B	�#B	�)B	�/B	�5B	�/B	�)B	�/B	�5B	�HB	�HB	�TB	�TB	�NB	�TB	�fB	�mB	�mB	�mB	�mB	�sB	�yB	�y@EB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
%B
%B
+B
+B
+B
1B
1B
	7B
	7B
	7B
1B
1B
1B
	7B
	7B

=B
DB
DB
DB
DB
DB
JB
JB
JB
PB
VB
VB
VB
VB
\B
\B
bB
bB
bB
hB
hB
�B
(>B
2�2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222224222222222222222222222222222222222222222222222222222222222222222222222222222 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.33 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005190539                              AO  ARCAADJP                                                                    20181005190539    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005190539  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005190539  QCF$                G�O�G�O�G�O�8000            