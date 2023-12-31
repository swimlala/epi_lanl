CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:05:05Z creation      
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
resolution        =���   axis      Z        d  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  @�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     d  B�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     d  K�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     d  ST   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  Z�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     d  \�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  c�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     d  e�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     d  m8   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  t�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     d  vx   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  }�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     d  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �L   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �L   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �L   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �L   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �x   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �|   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20181005190505  20181005190505  5904952 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               A   AO  6431                            2B  A   APEX                            7466                            062512                          846 @ל�KHy�1   @ל��b�L@3>vȴ9X�c�;dZ�1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      A   A   B   @�33@�  A��A!��A@  A`  A�  A�  A�  A�  A�  A�  A�  A�33A�33B  B  B��B��B(  B0  B8  B@  BHffBPffBX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C�fC  C   C"  C$  C&  C'�fC*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CU�fCX  CZ  C\  C^  C`  Cb  Cd  Cf�Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C��3C�  C�  C��C�  C�  C��C��C�  C�  C�  C��3C�  C��C�  C�  C��C��C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C��3C�  C�  C��3C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C��3C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C��C�  C�  C�  C��C�  C�  C��3C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C��3C�  C��C�  C��3C�  C��C��C�  C�  C��3C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C��3C�  C�  C�  D fD �fD  D� D  D� D  Dy�D��D� D  D� D  Dy�D��D� DfD� D	  D	� D
  D
� DfD� D  D� D  D�fD��Dy�D  D� D  Dy�D��Dy�D  D�fDfD�fD  D� D  D�fD  Dy�D��D� D  D� D  D� D  D� DfD� D  D� D��Dy�D  D�fD  D� D   D � D!  D!� D"  D"� D#  D#y�D$  D$� D%  D%� D&  D&y�D'  D'�fD(  D(� D4� D5  D5� D5��D6y�D6��D7� D8  D8� D9fD9�fD:fD:�fD;  D;� D<  D<� D<��D=y�D>  D>�fD?fD?�fD?��D@y�D@��DA� DBfDB� DC  DC� DC��DDy�DD��DEy�DF  DFy�DF��DGy�DH  DHy�DH��DIy�DJ  DJ� DK  DKy�DL  DL�fDM  DM�fDNfDN� DO  DO�fDP  DPy�DQ  DQ�fDR  DR� DS  DSy�DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[�fD\  D\�fD]  D]y�D^  D^�fD_  D_y�D`  D`� Da  Da� Db  Db�fDc  Dc�fDc��Dd�fDe  Dey�Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Djy�Dk  Dk� Dk��Dly�Dm  Dm� Dn  Dn� Dn��Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� DufDu� Dv  Dv� Dw  Dw� Dyg
D�2�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @{�@��\@�A�HA=G�A]G�A}G�A���A���A���A���AΣ�Aޣ�A��
A��
BQ�BQ�B�B�B'Q�B/Q�B7Q�B?Q�BG�RBO�RBWQ�B_Q�BgQ�BoQ�BwQ�BQ�B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���Bè�B��)B˨�BϨ�BӨ�Bר�Bۨ�Bߨ�B��B��B��B��B��B���B��)B���C�{C�{C�{C�{C	�{C�{C�{C�{C�{C�{C�{C�{C�{C��C�{C�{C!�{C#�{C%�{C'��C)�{C+�{C-�{C/�{C1�{C3�{C5�{C7�{C9�{C;�{C=�{C?�{CA�{CC�{CE�{CG�{CI�{CK�{CM�{CO�{CQ�{CS�{CU��CW�{CY�{C[�{C]�{C_�{Ca�{Cc�{Ce�Cg�{Ci�{Ck�{Cm�{Co�{Cq�{Cs�{Cu�{Cw�{Cy�{C{�{C}�{C�{C��=C��=C��=C��=C��=C��pC��=C��=C��
C��=C��=C��
C��
C��=C��=C��=C��pC��=C��
C��=C��=C��
C��
C��=C��pC��=C��=C��=C��=C��=C��=C��=C��=C��=C��
C��=C��=C��=C��=C��=C��pC��=C��=C��pC��=C��=C��=C��=C��=C��=C��pC��=C��=C��=C��=C��=C��=C��pC��=C��=C��=C��=C��=C��=C��pC��=C��
C��
C��=C��=C��=C��=C��=C��=C��=C��=C��pC��=C��
C��=C��=C��=C��
C��=C��=C��pC��=C��=C��=C��=C��=C��=C��pC��pC��=C��=C��pC��=C��
C��=C��pC��=C��
C��
C��=C��=C��pC��=C��=C��pC��=C��=C��=C��=C��=C��=C��=C��=C��
C��=C��=C��=C��=C��pC��=C��=C��=C��	D {�D �DuD�DuD�Dn�D�DuD�DuD�Dn�D�DuD��DuD�D	uD	�D
uD
��DuD�DuD�D{�D�Dn�D�DuD�Dn�D�Dn�D�D{�D��D{�D�DuD�D{�D�Dn�D�DuD�DuD�DuD�DuD��DuD�DuD�Dn�D�D{�D�DuD�D uD �D!uD!�D"uD"�D#n�D#�D$uD$�D%uD%�D&n�D&�D'{�D'�D(uD4uD4�D5uD5�D6n�D6�D7uD7�D8uD8��D9{�D9��D:{�D:�D;uD;�D<uD<�D=n�D=�D>{�D>��D?{�D?�D@n�D@�DAuDA��DBuDB�DCuDC�DDn�DD�DEn�DE�DFn�DF�DGn�DG�DHn�DH�DIn�DI�DJuDJ�DKn�DK�DL{�DL�DM{�DM��DNuDN�DO{�DO�DPn�DP�DQ{�DQ�DRuDR�DSn�DS�DTuDT�DUuDU�DVuDV�DWuDW�DXuDX�DYuDY�DZuDZ�D[{�D[�D\{�D\�D]n�D]�D^{�D^�D_n�D_�D`uD`�DauDa�Db{�Db�Dc{�Dc�Dd{�Dd�Den�De�DfuDf�DguDg�DhuDh�DiuDi�Djn�Dj�DkuDk�Dln�Dl�DmuDm�DnuDn�DouDo�DpuDp�DquDq�DruDr�DsuDs�DtuDt��DuuDu�DvuDv�DwuDy\)D�-11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Aˡ�A�$�Aʡ�A�l�A�`BA�VA�Q�A�K�A�=qA�;dA�A�A�G�A�K�A�K�A�Q�A�XA�\)A�bNA�n�A�l�A�l�A�jA�l�A�`BA�ZA�Q�A�K�A�A�A�7LA�+A�$�A�"�A� �A�{A���A��/Aɝ�A�{A�ZAƍPA�n�Aŧ�A�^5A�E�A�(�A�A��A���Aġ�AčPA�-A�AþwAÝ�A�M�A�?}A¥�A�ƨA���A���A�(�A��jA��A��A�JA�p�A��A��A�XA�oA�  A��^A��!A��mA��9A�~�A�~�A�;dA�
=A�C�A��jA���A�K�A��A���A�/A�\)A�bNA�jA�
=A��yA���A�ƨA�ĜA���A�O�A�hsA�l�A��A��hA���A�dZA�
=A�ȴA};dAx��Ar5?AqAnĜAk%Af~�Aa�wA];dAR�yAO
=AJ{AEC�A@��A?�wA>�/A<�A:^5A69XA5�A5+A3��A2��A2�\A1�wA/|�A+l�A)��A(r�A&��A%oA$ffA$bA#XA"9XA!�wA!&�A �A ��A ��A n�A =qA��A�hA�A�A�!A��A�A�A�FA7LA��A^5A(�A�A&�A�A�HA�A�9A��A��A��A��A�!A��A��A��A�A�-A9XA?}A
�`A
�A
bNA
n�A
��A
�A
��A
jA
ZA	S�Ax�A�AC�A �@��
@���@�
=@�J@�hs@��@���@���@���@�ȴ@��@�^@�1'@�+@�J@�^@�V@��@���@⟾@◍@�5?@���@�hs@�&�@�A�@އ+@���@���@��@�1'@��;@ۅ@�C�@���@��@�&�@�I�@�C�@ם�@�"�@�@��;@��T@�+@�E�@أ�@׾w@�C�@�l�@�
=@�ȴ@�v�@�X@��;@��@�n�@�M�@Ѳ-@�7L@���@�bN@��m@ϕ�@�"�@��@��@�5?@��@ͺ^@͉7@͑h@��`@�A�@��@�;d@�ff@�$�@��#@ɡ�@ɑh@�`B@ȃ@��
@�\)@�
=@�~�@��@���@�p�@Ĵ9@ēu@ēu@ă@�z�@�r�@�I�@��
@�K�@§�@�`B@��u@�j@�I�@� �@�ƨ@�dZ@��+@�V@��@�J@��7@��@���@��@��D@��@�z�@�Q�@� �@��w@��H@�ff@��T@�?}@���@�Q�@��w@��P@�\)@�ȴ@�@��h@�X@�&�@�Ĝ@�bN@� �@�l�@���@��@��j@�I�@�1'@���@�^5@�5?@�{@�x�@��D@��D@��D@��D@�Q�@��m@��F@�l�@�C�@�@�^5@�?}@��9@�z�@�(�@�l�@��!@�-@���@�hs@�O�@���@��D@�A�@�1'@��;@��;@���@��@�J@���@�x�@�`B@�O�@�?}@��D@�Q�@��@�@�=q@�@�p�@�9X@��w@�t�@�K�@�C�@�+@��!@��+@�V@�-@��T@�@��h@���@�1'@��;@�ƨ@���@�|�@�S�@��@��!@���@��\@�=q@���@��7@�X@�?}@���@�Ĝ@��9@���@���@�bN@�9X@���@�l�@��@���@��\@�^5@�^5@�=q@�5?@�$�@�J@��@���@�Ĝ@���@��u@�r�@�j@�bN@�Q�@�I�@�9X@��@�  @�ƨ@���@�t�@�S�@���@�ȴ@��+@�^5@���@���@��h@��@�`B@��@��@���@��9@��u@�  @��w@�dZ@�o@��y@�ȴ@���@��R@��!@���@���@�w�@q�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   Aˡ�A�$�Aʡ�A�l�A�`BA�VA�Q�A�K�A�=qA�;dA�A�A�G�A�K�A�K�A�Q�A�XA�\)A�bNA�n�A�l�A�l�A�jA�l�A�`BA�ZA�Q�A�K�A�A�A�7LA�+A�$�A�"�A� �A�{A���A��/Aɝ�A�{A�ZAƍPA�n�Aŧ�A�^5A�E�A�(�A�A��A���Aġ�AčPA�-A�AþwAÝ�A�M�A�?}A¥�A�ƨA���A���A�(�A��jA��A��A�JA�p�A��A��A�XA�oA�  A��^A��!A��mA��9A�~�A�~�A�;dA�
=A�C�A��jA���A�K�A��A���A�/A�\)A�bNA�jA�
=A��yA���A�ƨA�ĜA���A�O�A�hsA�l�A��A��hA���A�dZA�
=A�ȴA};dAx��Ar5?AqAnĜAk%Af~�Aa�wA];dAR�yAO
=AJ{AEC�A@��A?�wA>�/A<�A:^5A69XA5�A5+A3��A2��A2�\A1�wA/|�A+l�A)��A(r�A&��A%oA$ffA$bA#XA"9XA!�wA!&�A �A ��A ��A n�A =qA��A�hA�A�A�!A��A�A�A�FA7LA��A^5A(�A�A&�A�A�HA�A�9A��A��A��A��A�!A��A��A��A�A�-A9XA?}A
�`A
�A
bNA
n�A
��A
�A
��A
jA
ZA	S�Ax�A�AC�A �@��
@���@�
=@�J@�hs@��@���@���@���@�ȴ@��@�^@�1'@�+@�J@�^@�V@��@���@⟾@◍@�5?@���@�hs@�&�@�A�@އ+@���@���@��@�1'@��;@ۅ@�C�@���@��@�&�@�I�@�C�@ם�@�"�@�@��;@��T@�+@�E�@أ�@׾w@�C�@�l�@�
=@�ȴ@�v�@�X@��;@��@�n�@�M�@Ѳ-@�7L@���@�bN@��m@ϕ�@�"�@��@��@�5?@��@ͺ^@͉7@͑h@��`@�A�@��@�;d@�ff@�$�@��#@ɡ�@ɑh@�`B@ȃ@��
@�\)@�
=@�~�@��@���@�p�@Ĵ9@ēu@ēu@ă@�z�@�r�@�I�@��
@�K�@§�@�`B@��u@�j@�I�@� �@�ƨ@�dZ@��+@�V@��@�J@��7@��@���@��@��D@��@�z�@�Q�@� �@��w@��H@�ff@��T@�?}@���@�Q�@��w@��P@�\)@�ȴ@�@��h@�X@�&�@�Ĝ@�bN@� �@�l�@���@��@��j@�I�@�1'@���@�^5@�5?@�{@�x�@��D@��D@��D@��D@�Q�@��m@��F@�l�@�C�@�@�^5@�?}@��9@�z�@�(�@�l�@��!@�-@���@�hs@�O�@���@��D@�A�@�1'@��;@��;@���@��@�J@���@�x�@�`B@�O�@�?}@��D@�Q�@��@�@�=q@�@�p�@�9X@��w@�t�@�K�@�C�@�+@��!@��+@�V@�-@��T@�@��h@���@�1'@��;@�ƨ@���@�|�@�S�@��@��!@���@��\@�=q@���@��7@�X@�?}@���@�Ĝ@��9@���@���@�bN@�9X@���@�l�@��@���@��\@�^5@�^5@�=q@�5?@�$�@�J@��@���@�Ĝ@���@��u@�r�@�j@�bN@�Q�@�I�@�9X@��@�  @�ƨ@���@�t�@�S�@���@�ȴ@��+@�^5@���@���@��h@��@�`B@��@��@���@��9@��u@�  @��w@�dZ@�o@��y@�ȴ@���@��R@��!@���@���@�w�@q�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
�FB
�9B
�9B
�FB
�LB
�RB
�XB
�^B
�jB
�wB
��B
ÖB
ĜB
ĜB
ŢB
ǮB
ȴB
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�
B
�B
�)B
�/B
�5B
�;B
�HB
�ZB
�mB
�B+B%�B.B33B?}BJ�BN�B[#BiyBr�Bw�B|�B��B�LB�^BÖB�B�)B��BP�BB�B?}BXB^5BdZBiyBjBl�B^5BO�BF�BI�BR�BcTBe`B/B$�B�B%B�B�fB��B��B�'B�-B��B�%BdZBR�B)�B{B
��B
�sB
�)B
�5B
�HB
�;B
ÖB
��B
�bB
x�B
bNB
YB
VB
Q�B
=qB
�B	��B	�wB	�'B	��B	~�B	^5B	?}B	!�B�B�5B��B��BȴBĜB��B�XB�!B��B��B��B��B��B��B��B�bB�B�B}�Bz�B{�B{�B|�B� B�B�%B�1B�7B�7B�7B�=B�7B�7B�=B�\B�oB�\B�\B�PB�JB�=B�%B� B}�B{�Bp�Bk�BiyBiyBiyBhsBhsBhsBiyBiyBk�Bm�Bq�Bw�B�bB�dB�uB}�B�\B��B��B�FB��B��B�
B�
B�B�BǮB�dB�9B�'B��B��B�PB�DB�=B�1B�B�B~�B|�B{�Bz�B{�B�B�B�B�%B�=B�PB�VB�VB�\B�\B�\B�\B�bB�uB�{B�{B��B��B��B��B��B��B��B��B��B��B�B��B��B�#B�B	B	  B	  B	  B	B	+B	PB	\B	hB	�B	�B	�B	�B	�B	�B	�B	 �B	!�B	"�B	$�B	%�B	&�B	&�B	'�B	(�B	(�B	)�B	.B	1'B	49B	9XB	<jB	?}B	@�B	A�B	B�B	B�B	C�B	F�B	H�B	I�B	J�B	L�B	N�B	N�B	Q�B	XB	YB	YB	YB	YB	YB	YB	\)B	`BB	dZB	gmB	hsB	hsB	hsB	iyB	jB	l�B	p�B	r�B	s�B	s�B	v�B	x�B	z�B	z�B	{�B	{�B	|�B	~�B	�B	�B	�B	�%B	�1B	�DB	�PB	�bB	�oB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	�!B	�-B	�RB	�qB	�jB	�jA|�B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�B	�B	�#B	�)B	�/B	�)B	�#B	�B	�B	�
B	�B	��B	��B	��B	�B	��B	��B	�B	�)B	�/B	�/B	�;B	�;B	�;B	�;B	�;B	�;B	�;B	�;B	�5B	�;B	�NB	�TB	�ZB	�mB	�mB	�sB	�sB	�sB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
  B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
1B
	7B
	7B
	7B
	7B

=B

=B

=B

=B

=B

=B
DB
DB
JB
JB
JB
JB
JB
PB
VB
VB
bB
oB
uB
uB
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
%�22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222422222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222   B
�FB
�9B
�9B
�FB
�LB
�RB
�XB
�^B
�jB
�wB
��B
ÖB
ĜB
ĜB
ŢB
ǮB
ȴB
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�
B
�B
�)B
�/B
�5B
�;B
�HB
�ZB
�mB
�B+B%�B.B33B?}BJ�BN�B[#BiyBr�Bw�B|�B��B�LB�^BÖB�B�)B��BP�BB�B?}BXB^5BdZBiyBjBl�B^5BO�BF�BI�BR�BcTBe`B/B$�B�B%B�B�fB��B��B�'B�-B��B�%BdZBR�B)�B{B
��B
�sB
�)B
�5B
�HB
�;B
ÖB
��B
�bB
x�B
bNB
YB
VB
Q�B
=qB
�B	��B	�wB	�'B	��B	~�B	^5B	?}B	!�B�B�5B��B��BȴBĜB��B�XB�!B��B��B��B��B��B��B��B�bB�B�B}�Bz�B{�B{�B|�B� B�B�%B�1B�7B�7B�7B�=B�7B�7B�=B�\B�oB�\B�\B�PB�JB�=B�%B� B}�B{�Bp�Bk�BiyBiyBiyBhsBhsBhsBiyBiyBk�Bm�Bq�Bw�B�bB�dB�uB}�B�\B��B��B�FB��B��B�
B�
B�B�BǮB�dB�9B�'B��B��B�PB�DB�=B�1B�B�B~�B|�B{�Bz�B{�B�B�B�B�%B�=B�PB�VB�VB�\B�\B�\B�\B�bB�uB�{B�{B��B��B��B��B��B��B��B��B��B��B�B��B��B�#B�B	B	  B	  B	  B	B	+B	PB	\B	hB	�B	�B	�B	�B	�B	�B	�B	 �B	!�B	"�B	$�B	%�B	&�B	&�B	'�B	(�B	(�B	)�B	.B	1'B	49B	9XB	<jB	?}B	@�B	A�B	B�B	B�B	C�B	F�B	H�B	I�B	J�B	L�B	N�B	N�B	Q�B	XB	YB	YB	YB	YB	YB	YB	\)B	`BB	dZB	gmB	hsB	hsB	hsB	iyB	jB	l�B	p�B	r�B	s�B	s�B	v�B	x�B	z�B	z�B	{�B	{�B	|�B	~�B	�B	�B	�B	�%B	�1B	�DB	�PB	�bB	�oB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	�!B	�-B	�RB	�qB	�jB	�jA|�B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�B	�B	�#B	�)B	�/B	�)B	�#B	�B	�B	�
B	�B	��B	��B	��B	�B	��B	��B	�B	�)B	�/B	�/B	�;B	�;B	�;B	�;B	�;B	�;B	�;B	�;B	�5B	�;B	�NB	�TB	�ZB	�mB	�mB	�sB	�sB	�sB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
  B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
1B
	7B
	7B
	7B
	7B

=B

=B

=B

=B

=B

=B
DB
DB
JB
JB
JB
JB
JB
PB
VB
VB
bB
oB
uB
uB
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
%�22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222422222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=0.17 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005190505                              AO  ARCAADJP                                                                    20181005190505    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005190505  QCP$                G�O�G�O�G�O�F03E            AO  ARGQQCPL                                                                    20181005190505  QCF$                G�O�G�O�G�O�8000            