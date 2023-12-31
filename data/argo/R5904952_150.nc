CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:05:38Z creation      
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
_FillValue                    �@Argo profile    3.1 1.2 19500101000000  20181005190538  20181005190538  5904952 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6431                            2B  A   APEX                            7466                            062512                          846 @���[f�1   @����s�R@0�n��O��c��t�1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �A   A   A   @�33@�  A   A   A@  A^ffA~ffA�33A�  A�33A�  A���A���A�  B   BffB  B  B   B'��B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�33B�33B�33B�  B�  B�  B���B���B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C �C�C  C  C  C	�fC  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CA�fCD  CF  CH  CJ�CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~�C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C��3C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C��C�  C�  C��C�  C��3C�  C�  C�  C�  C��C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C��3C�  C�  C��3C�  C��C�  C��C�  D   D �fD  D� D  D� D  D� D  D� D  D� DfD� D  D� D  D� D	fD	�fD
  D
y�D
��D� D  Dy�D  D� D  D� D  D� D  D� D��D� DfD� D  Dy�D  D� DfD� D  D� D  D� D  D�fD  Dy�D  D� D  D� D  Dy�D  D�fD  D� D  D� D   D � D!  D!� D!��D"� D#fD#� D$  D$� D$��D%� D&  D&y�D'  D'� D(  D(� D)  D)� D*fD*� D+  D+� D,fD,� D-  D-� D.  D.� D/fD/�fD0  D0� D1  D1� D2  D2� D3  D3� D4fD4� D4��D5� D6  D6� D7  D7� D7��D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@�fDAfDA� DB  DB� DC  DC� DDfDD�fDE  DEy�DF  DF� DG  DGy�DH  DH� DI  DI� DI��DJ� DK  DK� DK��DL� DM  DM� DN  DNy�DO  DO� DP  DP�fDQ  DQy�DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[fD[� D\  D\� D]  D]� D^  D^� D_  D_y�D_��D`� DafDa� Db  Db� DcfDc�fDdfDd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Diy�Dj  Dj� DkfDk�fDl  Dl� Dm  Dm� Dm��Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr�fDs  Ds� DtfDt� Du  Du� Dv  Dvy�Dv��Dwy�Dw��Dy�RD�9�D��=1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�@ʏ\AG�A%G�AEG�Ac�A��
A��
A���A��
A£�A�p�A�p�A��BQ�B	�RBQ�BQ�B!Q�B(�B1Q�B9Q�BAQ�BIQ�BQQ�BYQ�BaQ�BiQ�BqQ�ByQ�B���B���B���B���B���B���B��)B��)B��)B���B���B���B�u�B�u�B���B���B���B��)BȨ�B̨�BШ�BԨ�Bب�Bܨ�B��B��B��B��B��B���B���B���C nCnCT{CT{CT{C
:�CT{CT{CT{CT{CT{CT{CT{CT{CT{CT{C T{C"T{C$T{C&T{C(T{C*T{C,T{C.T{C0T{C2T{C4T{C6T{C8T{C:T{C<T{C>T{C@T{CB:�CDT{CFT{CHT{CJnCLT{CNT{CPT{CRT{CTT{CVT{CXT{CZT{C\T{C^T{C`T{CbT{CdT{CfT{ChT{CjT{ClT{CnT{CpT{CrT{CtT{CvT{CxT{CzT{C|T{C~nC�7
C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�pC�pC�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�pC�*=C�*=C�*=C�*=C�7
C�7
C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�7
C�*=C�pC�*=C�*=C�*=C�*=C�*=C�7
C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�7
C�7
C�7
C�*=C�*=C�7
C�*=C�pC�*=C�*=C�*=C�*=C�7
C�*=C�pC�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�7
C�*=C�pC�*=C�*=C�pC�*=C�7
C�*=C�7
C�*=D D ��DD�DD�DD�DD�DD�D�D�DD�DD�D	�D	��D
D
��D�D�DD��DD�DD�DD�DD�D�D�D�D�DD��DD�D�D�DD�DD�DD��DD��DD�DD�DD��DD��DD�DD�D D �D!D!�D"�D"�D#�D#�D$D$�D%�D%�D&D&��D'D'�D(D(�D)D)�D*�D*�D+D+�D,�D,�D-D-�D.D.�D/�D/��D0D0�D1D1�D2D2�D3D3�D4�D4�D5�D5�D6D6�D7D7�D8�D8�D9D9�D:D:�D;D;�D<D<�D=D=�D>D>�D?D?�D@D@��DA�DA�DBDB�DCDC�DD�DD��DEDE��DFDF�DGDG��DHDH�DIDI�DJ�DJ�DKDK�DL�DL�DMDM�DNDN��DODO�DPDP��DQDQ��DRDR�DSDS�DTDT�DUDU�DVDV�DWDW�DXDX�DYDY�DZDZ�D[�D[�D\D\�D]D]�D^D^�D_D_��D`�D`�Da�Da�DbDb�Dc�Dc��Dd�Dd�DeDe�DfDf�DgDg�DhDh�DiDi��DjDj�Dk�Dk��DlDl�DmDm�Dn�Dn�DoDo�DpDp�DqDq�DrDr��DsDs�Dt�Dt�DuDu�DvDv��Dw�Dw��Dw��Dy�qD�D{D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�=qA�5?A�=qA�=qA�?}A�A�A�C�A�C�A�E�A�E�A�G�A�G�A�I�A�K�A�K�A�K�A�M�A�M�A�Q�A�S�A�VA�VA�XA�XA�XA�XA�XA�ZA�\)A�\)A�^5A�^5A�^5A�^5A�`BA�`BA�bNA�`BA�bNA�dZA�dZA�ffA�dZA�dZA�ffA�`BA�VA�VA�XA�XA�M�A�I�A�9XA���A�ffA��AθRA͕�A��;A���A���Ạ�A�{A�|�A�O�A�hsA��mA��9A�E�A��A�9XA��`A��PA��jA��-A�ĜA�"�A��HA�7LA�p�A�|�A���A�z�A��A��A��A��mA��hA�S�A��mA�A�A��hA��hA�1A��9A�9XA�;dA�hsA���A��A�A�C�A���A��PA�ZA�1A�
=A���A���A��A�bA�JA���A��mA�ȴA~�\Az�RAw"�AuO�Aq�AnVAkAj�AjI�Ah9XAc|�Aa�7A`M�A]|�AZ�9AWO�AS�7AR^5AQ�mAO�;ANZALAJjAG%ADVAC��A@�A<�9A7�^A5��A5x�A5C�A3XA17LA0 �A�A��An�AhsA��A�!A��A�
A+A�\A�A��AM�A�-A
�+AĜA&�A�A�^AoAbNA�PA�A�!A�DAjA��AA ��A �A�@�dZ@���@���@�o@���@��@��D@���@���@�n�@�l�@�@��@�j@�w@�^5@��@���@���@��@�|�@���@��@�dZ@��@�h@��@߶F@ݲ-@��@���@��m@�+@�t�@��@�  @���@�{@��#@�G�@�j@�/@���@ԃ@�I�@�\)@�J@���@�1@�S�@�~�@�$�@�
=@�=q@��/@�b@�t�@��@�-@���@ɑh@��@�Q�@��;@��@��@�hs@Ĭ@��
@��@��H@¸R@�@�~�@�^5@�$�@���@�X@�1@���@���@��#@�x�@�X@�X@�&�@�z�@��@���@��P@��y@���@��\@���@���@���@�=q@�{@�{@�-@�E�@�J@�x�@�/@��9@�I�@�9X@�r�@��@�A�@���@�dZ@�S�@�C�@�"�@��H@��H@���@�ȴ@�ȴ@�^5@��7@�r�@�1@� �@�  @�l�@���@�v�@�@�7L@��`@�bN@���@�|�@��@�
=@���@�`B@��@���@��u@�Q�@�1'@��m@�|�@�33@�+@�@��@���@�E�@���@���@��7@�p�@��@��u@�Z@�  @���@���@��w@���@���@��@�S�@���@���@�v�@��@��@��@��@��#@���@��7@�?}@�&�@�%@�Ĝ@�r�@��m@��@�|�@�o@���@�M�@��T@���@�hs@�7L@��/@�(�@�ƨ@�dZ@�"�@���@�~�@�=q@�@���@�X@���@�j@��m@�ƨ@���@��P@�+@��y@�ȴ@�~�@�ff@�V@�=q@�@�G�@��@��@��D@�j@� �@��w@�o@���@�=q@���@�p�@�7L@���@���@�bN@��@���@�l�@��@�v�@���@�&�@���@�z�@�1'@���@��@���@�\)@���@���@�V@���@�@�7L@�V@�%@���@���@�z�@��;@��@���@���@���@��P@�K�@�33@�"�@��@�@��@��H@���@�ff@��@�X@�7L@��@��@��@�z�@�Ĝ@��@��@��`@���@�Ĝ@��@���@�l�@�l�@�|�@��	@t,=@cO1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�=qA�5?A�=qA�=qA�?}A�A�A�C�A�C�A�E�A�E�A�G�A�G�A�I�A�K�A�K�A�K�A�M�A�M�A�Q�A�S�A�VA�VA�XA�XA�XA�XA�XA�ZA�\)A�\)A�^5A�^5A�^5A�^5A�`BA�`BA�bNA�`BA�bNA�dZA�dZA�ffA�dZA�dZA�ffA�`BA�VA�VA�XA�XA�M�A�I�A�9XA���A�ffA��AθRA͕�A��;A���A���Ạ�A�{A�|�A�O�A�hsA��mA��9A�E�A��A�9XA��`A��PA��jA��-A�ĜA�"�A��HA�7LA�p�A�|�A���A�z�A��A��A��A��mA��hA�S�A��mA�A�A��hA��hA�1A��9A�9XA�;dA�hsA���A��A�A�C�A���A��PA�ZA�1A�
=A���A���A��A�bA�JA���A��mA�ȴA~�\Az�RAw"�AuO�Aq�AnVAkAj�AjI�Ah9XAc|�Aa�7A`M�A]|�AZ�9AWO�AS�7AR^5AQ�mAO�;ANZALAJjAG%ADVAC��A@�A<�9A7�^A5��A5x�A5C�A3XA17LA0 �A�A��An�AhsA��A�!A��A�
A+A�\A�A��AM�A�-A
�+AĜA&�A�A�^AoAbNA�PA�A�!A�DAjA��AA ��A �A�@�dZ@���@���@�o@���@��@��D@���@���@�n�@�l�@�@��@�j@�w@�^5@��@���@���@��@�|�@���@��@�dZ@��@�h@��@߶F@ݲ-@��@���@��m@�+@�t�@��@�  @���@�{@��#@�G�@�j@�/@���@ԃ@�I�@�\)@�J@���@�1@�S�@�~�@�$�@�
=@�=q@��/@�b@�t�@��@�-@���@ɑh@��@�Q�@��;@��@��@�hs@Ĭ@��
@��@��H@¸R@�@�~�@�^5@�$�@���@�X@�1@���@���@��#@�x�@�X@�X@�&�@�z�@��@���@��P@��y@���@��\@���@���@���@�=q@�{@�{@�-@�E�@�J@�x�@�/@��9@�I�@�9X@�r�@��@�A�@���@�dZ@�S�@�C�@�"�@��H@��H@���@�ȴ@�ȴ@�^5@��7@�r�@�1@� �@�  @�l�@���@�v�@�@�7L@��`@�bN@���@�|�@��@�
=@���@�`B@��@���@��u@�Q�@�1'@��m@�|�@�33@�+@�@��@���@�E�@���@���@��7@�p�@��@��u@�Z@�  @���@���@��w@���@���@��@�S�@���@���@�v�@��@��@��@��@��#@���@��7@�?}@�&�@�%@�Ĝ@�r�@��m@��@�|�@�o@���@�M�@��T@���@�hs@�7L@��/@�(�@�ƨ@�dZ@�"�@���@�~�@�=q@�@���@�X@���@�j@��m@�ƨ@���@��P@�+@��y@�ȴ@�~�@�ff@�V@�=q@�@�G�@��@��@��D@�j@� �@��w@�o@���@�=q@���@�p�@�7L@���@���@�bN@��@���@�l�@��@�v�@���@�&�@���@�z�@�1'@���@��@���@�\)@���@���@�V@���@�@�7L@�V@�%@���@���@�z�@��;@��@���@���@���@��P@�K�@�33@�"�@��@�@��@��H@���@�ff@��@�X@�7L@��@��@��@�z�@�Ĝ@��@��@��`@���@�Ĝ@��@���@�l�@�l�@�|�@��	@t,=@cO1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B\)B\)B\)B\)B[#B[#B[#B[#B\)B\)B[#B\)B\)B[#B[#B\)B\)B\)B\)B\)B\)B\)B\)B\)B\)B\)B\)B\)B\)B\)B\)B\)B\)B\)B\)B\)B\)B\)B\)B\)B\)B\)B\)B\)B\)B]/B_;B`BB`BB`BB`BB`BBaHBbNBp�B�B�B�Bx�Bw�Bx�B~�B�jB�)B��B�B�B��B�B�BB�B<jBK�BVBe`BffBffBffBe`BaHBZBJ�BB�B<jB0!B�BB��B��B�B�sB�)B��B��B�!B�uB|�Bo�B]/BI�B �BBB
��B
�B
��B
�B
Q�B
G�B
@�B
33B
�B	��B	�B	�
B	�9B	��B	�oB	}�B	o�B	p�B	k�B	cTB	YB	C�B	6FB	-B	!�B	bB	JB��B��B�B�B�B�yB�BB�B��B��BɺB��B�XB�LB�FB�3B�'B�B�B��B��B��B��B��B��B��B�B�B��B�B�!B�3B�RB�dB�^B�RB�RB�XB�RB��BȴB��B��B��B�B�B�B�
B�5B�BB�B�/B�HB�ZB�ZB�ZB�B�B��B��B�B�B��B�B�HB�NB�`B�B�B�B�B�B�B�B�B�B�B�B�B�B�B��B��B	B	+B	
=B	
=B	
=B		7B	DB	hB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	"�B	/B	33B	6FB	5?B	7LB	8RB	9XB	;dB	<jB	>wB	B�B	D�B	E�B	I�B	K�B	M�B	N�B	Q�B	R�B	S�B	VB	YB	\)B	]/B	_;B	^5B	[#B	M�B	I�B	H�B	H�B	I�B	I�B	J�B	M�B	R�B	VB	ZB	\)B	_;B	ffB	iyB	iyB	jB	n�B	r�B	t�B	w�B	y�B	y�B	x�B	x�B	x�B	x�B	z�B	}�B	�B	�B	�B	�B	�B	�%B	�%B	�1B	�=B	�DB	�PB	�hB	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�'B	�-B	�3B	�-B	�3B	�3B	�3B	�3B	�?B	�FB	�FB	�RB	�XB	�^B	�qB	�qB	�qB	�qB	�qB	�wB	��B	B	ÖB	ÖB	ĜB	ĜB	ĜB	ÖB	ÖB	ÖB	ĜB	ŢB	ƨB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�#B	�#B	�B	�B	�B	�#B	�)B	�)B	�/B	�/B	�5B	�ZB	�fB	�fB	�`B	�`B	�`B	�fB	�fB	�mB	�mB	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
%B
+B
+B
1B
1B
	7B
	7B
	7B
	7B
	7B

=B
JB
PB
PB
PB
PB
PB
VB
\B
\B
bB
hB
hB
hB
hB
hB
oB
oB
oB
uB
uB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
)DB
4n2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 B\)B\)B\)B\)B[#B[#B[#B[#B\)B\)B[#B\)B\)B[#B[#B\)B\)B\)B\)B\)B\)B\)B\)B\)B\)B\)B\)B\)B\)B\)B\)B\)B\)B\)B\)B\)B\)B\)B\)B\)B\)B\)B\)B\)B\)B]/B_;B`BB`BB`BB`BB`BBaHBbNBp�B�B�B�Bx�Bw�Bx�B~�B�jB�)B��B�B�B��B�B�BB�B<jBK�BVBe`BffBffBffBe`BaHBZBJ�BB�B<jB0!B�BB��B��B�B�sB�)B��B��B�!B�uB|�Bo�B]/BI�B �BBB
��B
�B
��B
�B
Q�B
G�B
@�B
33B
�B	��B	�B	�
B	�9B	��B	�oB	}�B	o�B	p�B	k�B	cTB	YB	C�B	6FB	-B	!�B	bB	JB��B��B�B�B�B�yB�BB�B��B��BɺB��B�XB�LB�FB�3B�'B�B�B��B��B��B��B��B��B��B�B�B��B�B�!B�3B�RB�dB�^B�RB�RB�XB�RB��BȴB��B��B��B�B�B�B�
B�5B�BB�B�/B�HB�ZB�ZB�ZB�B�B��B��B�B�B��B�B�HB�NB�`B�B�B�B�B�B�B�B�B�B�B�B�B�B�B��B��B	B	+B	
=B	
=B	
=B		7B	DB	hB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	"�B	/B	33B	6FB	5?B	7LB	8RB	9XB	;dB	<jB	>wB	B�B	D�B	E�B	I�B	K�B	M�B	N�B	Q�B	R�B	S�B	VB	YB	\)B	]/B	_;B	^5B	[#B	M�B	I�B	H�B	H�B	I�B	I�B	J�B	M�B	R�B	VB	ZB	\)B	_;B	ffB	iyB	iyB	jB	n�B	r�B	t�B	w�B	y�B	y�B	x�B	x�B	x�B	x�B	z�B	}�B	�B	�B	�B	�B	�B	�%B	�%B	�1B	�=B	�DB	�PB	�hB	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�'B	�-B	�3B	�-B	�3B	�3B	�3B	�3B	�?B	�FB	�FB	�RB	�XB	�^B	�qB	�qB	�qB	�qB	�qB	�wB	��B	B	ÖB	ÖB	ĜB	ĜB	ĜB	ÖB	ÖB	ÖB	ĜB	ŢB	ƨB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�#B	�#B	�B	�B	�B	�#B	�)B	�)B	�/B	�/B	�5B	�ZB	�fB	�fB	�`B	�`B	�`B	�fB	�fB	�mB	�mB	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
%B
+B
+B
1B
1B
	7B
	7B
	7B
	7B
	7B

=B
JB
PB
PB
PB
PB
PB
VB
\B
\B
bB
hB
hB
hB
hB
hB
oB
oB
oB
uB
uB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
)DB
4n2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.33 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005190538                              AO  ARCAADJP                                                                    20181005190538    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005190538  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005190538  QCF$                G�O�G�O�G�O�8000            