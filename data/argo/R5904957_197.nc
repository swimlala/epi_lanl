CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-24T14:08:41Z creation      
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
resolution        =���   axis      Z          9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  @�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���       BD   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  IT   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���       K   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       R(   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  Y8   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       Z�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  b   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       c�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       j�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  q�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       s�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  z�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       |�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �    HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �D   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �T   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �X   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �h   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �l   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �p   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �tArgo profile    3.1 1.2 19500101000000  20181024140841  20181024140841  5904957 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6560                            2B  A   APEX                            7471                            062512                          846 @��䠝׹1   @���+�v�@5I7KƧ��d;dZ�1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �A   A   B   @�33@�  A   A   A@  A`  A�  A�  A�  A���A�33A�33A�  A���B   B��B  B  B   B(  B0  B7��B?��BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�33B�  C   C  C  C  C  C
  C  C  C  C�fC  C  C�fC  C  C  C   C"  C$  C&  C(  C*  C,  C-�fC0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cm�fCp  Cr�Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C��3C�  C�  C�  C�  C��C��C�  C�  C��C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C��C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C��C��C��C�  C��3C�  D   D � D  D� D  D� D  D� D  D� D��Dy�D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D  D� D  Dy�D  D� D��Dy�D  D� D  D� D  D� D  D�fDfD� D  D� D  D� D  D� D   D � D!  D!� D!��D"� D.� D.��D/y�D0  D0� D1  D1� D2  D2� D3fD3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D8��D9y�D:  D:� D;  D;� D<  D<y�D=  D=� D>  D>� D?fD?� D@  D@� DA  DA� DB  DB� DB��DC� DC��DD� DEfDE� DF  DF� DGfDG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN�fDOfDO� DO��DP� DQfDQ�fDR  DR� DS  DS� DT  DTy�DU  DU� DV  DV�fDWfDW� DX  DX� DY  DYy�DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D_��D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg�fDh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dw��Dy�fD�:=D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@�@ʏ\AG�A%G�AEG�AeG�A���A���A���A�p�A��
A��
A��A�p�BQ�B�BQ�BQ�B!Q�B)Q�B1Q�B8�B@�BIQ�BQQ�BYQ�BaQ�BiQ�BqQ�ByQ�Bܨ�B��B��B��B��B��B���B��)B���C T{CT{CT{CT{CT{C
T{CT{CT{CT{C:�CT{CT{C:�CT{CT{CT{C T{C"T{C$T{C&T{C(T{C*T{C,T{C.:�C0T{C2T{C4T{C6T{C8T{C:T{C<T{C>T{C@T{CBT{CDT{CFT{CHT{CJT{CLT{CNT{CPT{CRT{CTT{CVT{CXT{CZT{C\T{C^T{C`T{CbT{CdT{CfT{ChT{CjT{ClT{Cn:�CpT{CrnCtT{CvT{CxT{CzT{C|T{C~T{C�*=C�*=C�*=C�*=C�*=C�7
C�*=C�*=C�*=C�*=C�*=C�pC�*=C�*=C�*=C�*=C�*=C�*=C�7
C�7
C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�pC�pC�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�pC�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�pC�pC�*=C�pC�*=C�*=C�*=C�*=C�7
C�7
C�*=C�*=C�7
C�*=C�*=C�*=C�*=C�7
C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�7
C�*=C�7
C�7
C�7
C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�7
C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�pC�*=C�*=C�*=C�*=C�*=C�*=C�7
C�7
C�7
C�*=C�pC�*=D D �DD�DD�DD�DD�D�D��DD�DD�DD�D	D	�D
D
�DD�DD�DD�DD�DD�DD�DD�DD��DD�DD�DD��DD�D�D��DD�DD�DD�DD��D�D�DD�DD�DD�D D �D!D!�D"�D"�D.�D/�D/��D0D0�D1D1�D2D2�D3�D3�D4D4�D5D5�D6D6�D7D7�D8D8�D9�D9��D:D:�D;D;�D<D<��D=D=�D>D>�D?�D?�D@D@�DADA�DBDB�DC�DC�DD�DD�DE�DE�DFDF�DG�DG�DHDH�DIDI�DJDJ�DKDK�DLDL�DMDM�DNDN��DO�DO�DP�DP�DQ�DQ��DRDR�DSDS�DTDT��DUDU�DVDV��DW�DW�DXDX�DYDY��DZDZ�D[D[�D\D\�D]D]�D^D^�D_D_�D`�D`�DaDa�DbDb�DcDc�DdDd�DeDe�DfDf�DgDg��DhDh�DiDi�DjDj�DkDk�DlDl�DmDm�DnDn�DoDo�DpDp�DqDq�DrDr�DsDs�DtDt�DuDu�DvDv�DwDw�Dx�Dy��D�D�D��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�5?A�1'A�+A��A�A�A���A΅AΉ7AΟ�A�A�oA�oA�A��AΩ�A΅AΗ�Aΰ!AΟ�A΁A�r�A�p�A�n�A�ffA�`BA�^5A�^5A�^5A�ZA�S�A� �A�v�A���A���A�O�A��HA�|�A���A���A�|�A�\)A��-A�`BA���A�JA���A��wA��jA�l�A���A��jA��A��A��7A�1A���A�A�7LA���A�5?A��;A�;dA�x�A�dZA��A��A���A��HA�n�A��A�VA���A�?}A��TA��PA���A�S�A~=qA}��A|~�Ax��Av�AuhsAt��Aq�Ap�ApI�An�yAm�;Ak��Ai��Ah�!Ag�FAgoAe�hAcAbbNAb-Aa
=A^�jA]�A]oA\I�A[ƨAZ�AWt�AVAT��ASK�AR�AQ��AP�AP$�ANQ�ALz�AJ$�AG�AD�AC�;AC�AC�AA�FA?�hA=��A<�DA<�A;C�A9��A7�A6z�A6M�A6-A6JA5��A4�RA1VA0�A/O�A.��A.��A.VA-&�A*�/A)�;A(�9A'��A'��A'`BA&9XA#�wA"�RA"ffA"5?A!�mA �AȴA|�A�A9XA?}A
=A��A�`A��A�\A �AƨA�9A�jAbA�#A��A��AdZA�uAr�A�Az�AoA�
A��A1'A
�\A	�^A��A��A`BAAn�A�
A&�A��A �A`BA%A ��A �yA ��A Q�@�dZ@�`B@��@�X@���@�33@��@�7L@��@�j@�bN@�(�@�|�@�o@�n�@��@�7L@�\)@��@畁@���@��@�O�@�dZ@�ff@��`@�&�@�j@�I�@�(�@��@�dZ@���@ޟ�@���@�X@�%@��`@���@�Ĝ@�  @�$�@� �@�K�@�x�@ԋD@�A�@�33@д9@��@Ͼw@ϕ�@�|�@�\)@��@�@�~�@�`B@��@�33@�hs@�=q@�Z@å�@���@�O�@���@���@�-@�@��^@���@���@���@��h@�J@�;d@�5?@�p�@��+@�p�@��@�b@��@�M�@�J@���@�I�@��w@�|�@�\)@�o@���@�ff@�@�$�@�M�@�~�@��\@��R@��@��@��!@��!@��!@��!@��!@��!@��R@��R@���@���@��@���@��H@��y@��y@��H@���@�5?@���@��w@�"�@���@�V@��9@�bN@�b@��@���@��@�E�@��T@�`B@���@�|�@�+@�"�@���@�~�@�^5@�^5@�E�@�$�@�-@��T@��^@�x�@�?}@��9@��@�z�@��F@�5?@��@�E�@�$�@�%@�I�@�9X@���@���@��w@��H@�@���@�@���@�p�@�x�@��@�x�@�p�@��@�bN@� �@�b@�  @��
@��@�C�@�o@���@��@�v�@�$�@�{@�{@�{@���@��@��#@���@���@��7@�x�@�X@�?}@�V@���@���@���@��@�r�@�Z@��@��w@�K�@���@�5?@�@�X@��@�%@��j@��9@���@�r�@�bN@�1'@��;@��P@�@���@��\@�~�@��\@��!@��+@�V@�{@��@�$�@��@�J@�@�-@�V@�E�@��@���@�@��@���@���@��@��`@��@��@�V@�}V@��z@o��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�5?A�1'A�+A��A�A�A���A΅AΉ7AΟ�A�A�oA�oA�A��AΩ�A΅AΗ�Aΰ!AΟ�A΁A�r�A�p�A�n�A�ffA�`BA�^5A�^5A�^5A�ZA�S�A� �A�v�A���A���A�O�A��HA�|�A���A���A�|�A�\)A��-A�`BA���A�JA���A��wA��jA�l�A���A��jA��A��A��7A�1A���A�A�7LA���A�5?A��;A�;dA�x�A�dZA��A��A���A��HA�n�A��A�VA���A�?}A��TA��PA���A�S�A~=qA}��A|~�Ax��Av�AuhsAt��Aq�Ap�ApI�An�yAm�;Ak��Ai��Ah�!Ag�FAgoAe�hAcAbbNAb-Aa
=A^�jA]�A]oA\I�A[ƨAZ�AWt�AVAT��ASK�AR�AQ��AP�AP$�ANQ�ALz�AJ$�AG�AD�AC�;AC�AC�AA�FA?�hA=��A<�DA<�A;C�A9��A7�A6z�A6M�A6-A6JA5��A4�RA1VA0�A/O�A.��A.��A.VA-&�A*�/A)�;A(�9A'��A'��A'`BA&9XA#�wA"�RA"ffA"5?A!�mA �AȴA|�A�A9XA?}A
=A��A�`A��A�\A �AƨA�9A�jAbA�#A��A��AdZA�uAr�A�Az�AoA�
A��A1'A
�\A	�^A��A��A`BAAn�A�
A&�A��A �A`BA%A ��A �yA ��A Q�@�dZ@�`B@��@�X@���@�33@��@�7L@��@�j@�bN@�(�@�|�@�o@�n�@��@�7L@�\)@��@畁@���@��@�O�@�dZ@�ff@��`@�&�@�j@�I�@�(�@��@�dZ@���@ޟ�@���@�X@�%@��`@���@�Ĝ@�  @�$�@� �@�K�@�x�@ԋD@�A�@�33@д9@��@Ͼw@ϕ�@�|�@�\)@��@�@�~�@�`B@��@�33@�hs@�=q@�Z@å�@���@�O�@���@���@�-@�@��^@���@���@���@��h@�J@�;d@�5?@�p�@��+@�p�@��@�b@��@�M�@�J@���@�I�@��w@�|�@�\)@�o@���@�ff@�@�$�@�M�@�~�@��\@��R@��@��@��!@��!@��!@��!@��!@��!@��R@��R@���@���@��@���@��H@��y@��y@��H@���@�5?@���@��w@�"�@���@�V@��9@�bN@�b@��@���@��@�E�@��T@�`B@���@�|�@�+@�"�@���@�~�@�^5@�^5@�E�@�$�@�-@��T@��^@�x�@�?}@��9@��@�z�@��F@�5?@��@�E�@�$�@�%@�I�@�9X@���@���@��w@��H@�@���@�@���@�p�@�x�@��@�x�@�p�@��@�bN@� �@�b@�  @��
@��@�C�@�o@���@��@�v�@�$�@�{@�{@�{@���@��@��#@���@���@��7@�x�@�X@�?}@�V@���@���@���@��@�r�@�Z@��@��w@�K�@���@�5?@�@�X@��@�%@��j@��9@���@�r�@�bN@�1'@��;@��P@�@���@��\@�~�@��\@��!@��+@�V@�{@��@�$�@��@�J@�@�-@�V@�E�@��@���@�@��@���@���@��@��`@��@��@�V@�}V@��z@o��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
�;B
�;B
�5B
�5B
�#B
��B �B>wBE�B]/BcTBffBdZB`BBS�BO�B^5BjBl�Bn�Bp�Bp�Bq�Bq�Bq�Bq�Br�Bu�B{�B}�B�B�=B�=B�=B�=B�=B�1B�B}�Bt�B[#BN�BI�BE�BB�BC�BC�BC�B@�B49B6FB0!B%�B�B�BB��B�B�B��BĜB�'B�BjBQ�B@�B!�BDB
�B
�/B
��B
�?B
�{B
�JB
�B
k�B
R�B
C�B
?}B
2-B

=B	��B	�B	�`B	��B	��B	ǮB	��B	�qB	�-B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�uB	�\B	�=B	�B	�B	w�B	l�B	dZB	\)B	VB	O�B	L�B	F�B	A�B	9XB	/B	$�B	�B	1B	B��B��B�B�mB�;B�#B�
B��B��BǮBÖBÖBB��B�}B�^B�RB�9B�3B�'B�'B�!B�B��B��B��B��B��B��B��B��B��B��B�{B�hB�JB�=B�%B�B�B�B�B�B�B�B�B�B� B�B}�By�Bw�Bv�Bu�Bq�Bl�BgmBffBbNB^5B^5B^5B\)B^5B^5B]/B]/B\)B[#BZBZBYBZB_;BcTBdZBffBgmBiyBs�Bx�Bx�Bq�BjBgmBgmBhsBk�Bm�Bm�Bn�Bp�Bs�Bu�Bt�Bu�Bw�By�B�B�1B�7B�1B�B�B�B~�B�PB��B��B��B�{B�oB�uB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�9B�LB�FB�B��B��B��B�uB�uB�hB�\B�PB�JB�JB�JB�DB�DB�7B�\B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B��BɺB��B��B�B�)B�;B&�B	v�B	w�B	w�B	w�B	w�B	x�B	{�B	{�B	|�B	|�B	}�B	|�B	}�B	~�B	~�B	� B	� B	~�B	~�B	}�B	|�B	{�B	~�B	~�B	}�B	� B	�B	�B	�B	�B	�B	�B	�+B	�JB	�VB	�VB	�hB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�!B	�3B	�9B	�9B	�9B	�-B	�9B	�^B	�}B	�}B	�qB	�}B	��B	��B	B	B	ĜB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�B	�B	�B	�B	�#B	�)B	�/B	�/B	�5B	�5B	�HB	�TB	�ZB	�`B	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
+B
	7B
�B
�B
%�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B
�;B
�;B
�5B
�5B
�#B
��B �B>wBE�B]/BcTBffBdZB`BBS�BO�B^5BjBl�Bn�Bp�Bp�Bq�Bq�Bq�Bq�Br�Bu�B{�B}�B�B�=B�=B�=B�=B�=B�1B�B}�Bt�B[#BN�BI�BE�BB�BC�BC�BC�B@�B49B6FB0!B%�B�B�BB��B�B�B��BĜB�'B�BjBQ�B@�B!�BDB
�B
�/B
��B
�?B
�{B
�JB
�B
k�B
R�B
C�B
?}B
2-B

=B	��B	�B	�`B	��B	��B	ǮB	��B	�qB	�-B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�uB	�\B	�=B	�B	�B	w�B	l�B	dZB	\)B	VB	O�B	L�B	F�B	A�B	9XB	/B	$�B	�B	1B	B��B��B�B�mB�;B�#B�
B��B��BǮBÖBÖBB��B�}B�^B�RB�9B�3B�'B�'B�!B�B��B��B��B��B��B��B��B��B��B��B�{B�hB�JB�=B�%B�B�B�B�B�B�B�B�B�B� B�B}�By�Bw�Bv�Bu�Bq�Bl�BgmBffBbNB^5B^5B^5B\)B^5B^5B]/B]/B\)B[#BZBZBYBZB_;BcTBdZBffBgmBiyBs�Bx�Bx�Bq�BjBgmBgmBhsBk�Bm�Bm�Bn�Bp�Bs�Bu�Bt�Bu�Bw�By�B�B�1B�7B�1B�B�B�B~�B�PB��B��B��B�{B�oB�uB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�9B�LB�FB�B��B��B��B�uB�uB�hB�\B�PB�JB�JB�JB�DB�DB�7B�\B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B��BɺB��B��B�B�)B�;B&�B	v�B	w�B	w�B	w�B	w�B	x�B	{�B	{�B	|�B	|�B	}�B	|�B	}�B	~�B	~�B	� B	� B	~�B	~�B	}�B	|�B	{�B	~�B	~�B	}�B	� B	�B	�B	�B	�B	�B	�B	�+B	�JB	�VB	�VB	�hB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�!B	�3B	�9B	�9B	�9B	�-B	�9B	�^B	�}B	�}B	�qB	�}B	��B	��B	B	B	ĜB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�B	�B	�B	�B	�#B	�)B	�/B	�/B	�5B	�5B	�HB	�TB	�ZB	�`B	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
+B
	7B
�B
�B
%�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.33 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181024140841                              AO  ARCAADJP                                                                    20181024140841    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181024140841  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181024140841  QCF$                G�O�G�O�G�O�0               