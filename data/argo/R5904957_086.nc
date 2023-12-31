CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-24T14:08:20Z creation      
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
_FillValue                 �  A0   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C    PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  T�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \P   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ^@   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  f    TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  g�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  o�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  wp   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  y`   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �    SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �    SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �    SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �    HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �,   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �0   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �4   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �8   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �<   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �|   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20181024140820  20181024140820  5904957 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               VA   AO  6560                            2B  A   APEX                            7471                            062512                          846 @��$k$�1   @��$��t�@3/\(��c�V�u1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      VA   A   A   @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B ��B'��B0  B8  B@  BG��BP  BX  B`  Bh  Bp  BxffB�33B�33B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B���B�  B�  B�  B�  B�  B�  B���C   C�fC�fC  C  C
  C  C�C  C�fC  C  C  C  C  C  C   C"  C$  C&�C(�C*  C,  C.  C0  C2  C4  C6  C8  C:  C<�C>  C?�fCA�fCD  CF  CH  CJ  CL  CN�CP  CR  CT  CV  CW�fCZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx�Cz  C|  C}�fC�fC�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C��C�  C�  C�  C��3C��3C�  C�  C�  C�  C��C�  C�  C�  C��C��C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C��C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  D   D � D  D� D  D� D��D� D  D� D  D�fDfD� D  D� D  D� D��D	� D
  D
� DfD� D  D� D  D� D  D� D  D� D  D� DfD� D  D� D  D� D  D� D  D� D  D� D  D� D��Dy�D  D� D  D� D  D� D  D� D  Dy�D  D� D��Dy�D   D � D!  D!� D!��D"y�D#  D#� D$  D$� D%  D%� D&  D&� D'  D'�fD(fD(�fD)fD)�fD*fD*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0fD0�fD1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9fD9� D:  D:� D;  D;� D<  D<� D=  D=y�D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DDy�DE  DE� DF  DFy�DF��DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL�fDMfDM� DN  DN� DO  DO� DP  DP� DQfDQ� DR  DRy�DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  Dey�Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds�fDt  Dt� Du  Du� Dv  Dv� Dw  Dwy�DynD�D�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��\@ʏ\AG�A%G�AEG�AeG�A���A���A���A���A£�Aң�A��A��BQ�B	Q�BQ�BQ�B"�B(�B1Q�B9Q�BAQ�BH�BQQ�BYQ�BaQ�BiQ�BqQ�By�RB��)B��)B�u�B���B���B���B���B���B���B���B���B���B��)B���B���B���B���BĨ�BȨ�B̨�BШ�BԨ�Bب�B�u�B�u�B��B��B��B��B���B���B�u�C T{C:�C:�CT{CT{C
T{CT{CnCT{C:�CT{CT{CT{CT{CT{CT{C T{C"T{C$T{C&nC(nC*T{C,T{C.T{C0T{C2T{C4T{C6T{C8T{C:T{C<nC>T{C@:�CB:�CDT{CFT{CHT{CJT{CLT{CNnCPT{CRT{CTT{CVT{CX:�CZT{C\T{C^T{C`T{CbT{CdT{CfT{ChT{CjT{ClT{CnT{CpT{CrT{CtT{CvT{CxnCzT{C|T{C~:�C�pC�*=C�*=C�*=C�*=C�*=C�*=C�pC�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�7
C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�pC�*=C�*=C�*=C�*=C�*=C�7
C�*=C�*=C�*=C�pC�pC�*=C�*=C�*=C�*=C�7
C�*=C�*=C�*=C�7
C�7
C�*=C�*=C�*=C�*=C�7
C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�pC�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�pC�*=C�7
C�*=C�*=C�*=C�*=C�*=C�*=C�pC�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�7
C�*=C�*=D D �DD�DD�D�D�DD�DD��D�D�DD�DD�D	�D	�D
D
�D�D�DD�DD�DD�DD�DD�D�D�DD�DD�DD�DD�DD�DD�D�D��DD�DD�DD�DD�DD��DD�D�D��D D �D!D!�D"�D"��D#D#�D$D$�D%D%�D&D&�D'D'��D(�D(��D)�D)��D*�D*�D+D+�D,D,�D-D-�D.D.�D/D/�D0�D0��D1D1�D2D2�D3D3�D4D4�D5D5�D6D6�D7D7�D8D8�D9�D9�D:D:�D;D;�D<D<�D=D=��D>D>�D?D?�D@D@�DADA�DBDB�DCDC�DDDD��DEDE�DFDF��DG�DG�DHDH�DIDI�DJDJ�DKDK�DLDL��DM�DM�DNDN�DODO�DPDP�DQ�DQ�DRDR��DSDS�DTDT�DUDU�DVDV�DWDW�DXDX�DYDY�DZDZ�D[D[�D\D\�D]D]�D^D^�D_D_�D`D`�DaDa�DbDb�DcDc�DdDd�DeDe��DfDf�DgDg�DhDh�DiDi�DjDj�DkDk�DlDl�DmDm�DnDn�DoDo�DpDp�DqDq�DrDr�DsDs��DtDt�DuDu�DvDv�DwDw��Dy�3D�O\1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�S�A�bA݋DA� �A�JA��A��TA���A���A�ȴA�ĜAܺ^Aܴ9Aܧ�A܍PA�VA���A�jA���A�&�A�A��;A̩�AʍPA�A�A��yAɰ!A�~�A�C�A�(�A�VAǾwA��A�ĜA�&�A��`A���A�  A���A� �A�/A�{A���A�A���A�&�A��;A�5?A��yA���A��HA�O�A���A���A�A�;dA�ĜA�G�A���A�C�A�t�A��A��`A��#A��TA�dZA���A��-A�p�A��/A�`BA�(�A���A�t�A�E�A}AwhsAu�As�^Amp�Ak"�Aix�Ag+Ad��Abr�AaO�A`��A^�jA]�A[�
AZ�HAXz�ATVASXAQ�7AM�TAL�9AJȴAH��AHM�AFjAB�+AA�-AA�A@v�A@1'A@JA?�TA?�^A?x�A>~�A:5?A9��A9;dA8��A8bNA5��A4�\A3��A0ĜA/VA-��A,�\A*�yA'�wA'"�A&��A%+A#�A"�A"�A"z�A"n�A"I�A!��A!C�A �A r�A��A�AA9XA��A&�Ar�A7LA��AS�AƨA�A��Ap�A7LAVA��A��A1Ap�A
=A��A�;A
ȴA	�A��AZAv�A�AJA"�A��A��A �uA VA =qA $�A �@��
@�l�@��!@���@�%@���@�F@�X@�@�9X@��;@�ƨ@�"�@��@�33@�=q@�@�ȴ@柾@�7L@�Ĝ@�@� �@��@�l�@�-@�"�@�J@�;d@�O�@�%@��/@أ�@�I�@ץ�@���@�/@�V@��`@�Ĝ@� �@҇+@Гu@�%@̓u@˝�@�33@�|�@�
=@ʸR@�ff@���@�hs@���@�  @Ł@�O�@őh@�Ĝ@öF@�{@�J@��y@�b@�p�@Ɨ�@�X@��h@��h@��/@�n�@�A�@��F@�dZ@�o@��H@��H@���@�~�@�E�@�^5@�^5@�5?@�J@���@��@��T@���@���@���@��@�p�@�`B@�G�@��@�%@��@��`@���@���@�Ĝ@��j@��9@���@�  @�;d@�@���@�~�@�{@��@��@�`B@���@��j@��F@�C�@�"�@�@��y@���@�M�@�{@��h@�p�@�x�@��@���@��@��
@�+@�ȴ@���@�ff@���@��7@�V@���@���@��`@��
@�+@�
=@�n�@��h@�G�@��@��m@���@�ƨ@��D@�V@�@��@���@��
@��@�A�@�+@��R@�n�@�^5@�n�@��@�X@�%@���@�I�@��@��F@��F@���@���@���@���@���@���@��@�;d@���@��@�hs@��@��/@��u@��P@�"�@���@�ȴ@�n�@�x�@��j@�b@�dZ@�t�@���@�\)@�+@�"�@�v�@��@��#@�G�@���@��9@���@��@�z�@�j@�Z@��@�(�@��@�(�@�A�@� �@�b@�b@�b@��@�|�@�33@��@�@���@���@��y@���@�~�@���@���@�/@�Ĝ@��9@���@�z�@�Z@���@�o@��\@�M�@�-@��T@�@�@���@��@�G�@�/@�/@�/@�/@�/@�/@�/@�&�@�V@���@�Q�@�(�@�b@��@��@��
@���@��w@��F@��@���@�\)@�;d@�+@��@�
=@�@��H@���@�v�@�=q@��@���@���@��@�hs@�`B@�?}@��@��@�Ĝ@���@�Q�@���@��F@��@�S�@�o@���@�E�@�J@��T@���@��h@�V@���@��D@�I�@���@��@�;d@��!@��\@�=q@��@�{@�@���@��h@��@���@jO1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�S�A�bA݋DA� �A�JA��A��TA���A���A�ȴA�ĜAܺ^Aܴ9Aܧ�A܍PA�VA���A�jA���A�&�A�A��;A̩�AʍPA�A�A��yAɰ!A�~�A�C�A�(�A�VAǾwA��A�ĜA�&�A��`A���A�  A���A� �A�/A�{A���A�A���A�&�A��;A�5?A��yA���A��HA�O�A���A���A�A�;dA�ĜA�G�A���A�C�A�t�A��A��`A��#A��TA�dZA���A��-A�p�A��/A�`BA�(�A���A�t�A�E�A}AwhsAu�As�^Amp�Ak"�Aix�Ag+Ad��Abr�AaO�A`��A^�jA]�A[�
AZ�HAXz�ATVASXAQ�7AM�TAL�9AJȴAH��AHM�AFjAB�+AA�-AA�A@v�A@1'A@JA?�TA?�^A?x�A>~�A:5?A9��A9;dA8��A8bNA5��A4�\A3��A0ĜA/VA-��A,�\A*�yA'�wA'"�A&��A%+A#�A"�A"�A"z�A"n�A"I�A!��A!C�A �A r�A��A�AA9XA��A&�Ar�A7LA��AS�AƨA�A��Ap�A7LAVA��A��A1Ap�A
=A��A�;A
ȴA	�A��AZAv�A�AJA"�A��A��A �uA VA =qA $�A �@��
@�l�@��!@���@�%@���@�F@�X@�@�9X@��;@�ƨ@�"�@��@�33@�=q@�@�ȴ@柾@�7L@�Ĝ@�@� �@��@�l�@�-@�"�@�J@�;d@�O�@�%@��/@أ�@�I�@ץ�@���@�/@�V@��`@�Ĝ@� �@҇+@Гu@�%@̓u@˝�@�33@�|�@�
=@ʸR@�ff@���@�hs@���@�  @Ł@�O�@őh@�Ĝ@öF@�{@�J@��y@�b@�p�@Ɨ�@�X@��h@��h@��/@�n�@�A�@��F@�dZ@�o@��H@��H@���@�~�@�E�@�^5@�^5@�5?@�J@���@��@��T@���@���@���@��@�p�@�`B@�G�@��@�%@��@��`@���@���@�Ĝ@��j@��9@���@�  @�;d@�@���@�~�@�{@��@��@�`B@���@��j@��F@�C�@�"�@�@��y@���@�M�@�{@��h@�p�@�x�@��@���@��@��
@�+@�ȴ@���@�ff@���@��7@�V@���@���@��`@��
@�+@�
=@�n�@��h@�G�@��@��m@���@�ƨ@��D@�V@�@��@���@��
@��@�A�@�+@��R@�n�@�^5@�n�@��@�X@�%@���@�I�@��@��F@��F@���@���@���@���@���@���@��@�;d@���@��@�hs@��@��/@��u@��P@�"�@���@�ȴ@�n�@�x�@��j@�b@�dZ@�t�@���@�\)@�+@�"�@�v�@��@��#@�G�@���@��9@���@��@�z�@�j@�Z@��@�(�@��@�(�@�A�@� �@�b@�b@�b@��@�|�@�33@��@�@���@���@��y@���@�~�@���@���@�/@�Ĝ@��9@���@�z�@�Z@���@�o@��\@�M�@�-@��T@�@�@���@��@�G�@�/@�/@�/@�/@�/@�/@�/@�&�@�V@���@�Q�@�(�@�b@��@��@��
@���@��w@��F@��@���@�\)@�;d@�+@��@�
=@�@��H@���@�v�@�=q@��@���@���@��@�hs@�`B@�?}@��@��@�Ĝ@���@�Q�@���@��F@��@�S�@�o@���@�E�@�J@��T@���@��h@�V@���@��D@�I�@���@��@�;d@��!@��\@�=q@��@�{@�@���@��h@��@���@jO1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�7B�%B�B�B�B�B�B�B�B�B�B�B�B� B~�B{�By�Bw�Bu�B�+B�JB��B�-B�yB�B�B�B��B��B��B��B\BL�BC�B=qB:^B33B0!B-B0!B5?B2-B.B �B{B\BDB+BBB��B�sBB��B��B�1Bz�Bt�Bo�BhsBcTB]/BQ�BB�B+B�BVB
��B
�B
�mB
ɺB
��B
v�B
B�B
 �B
B	�;B	��B	�^B	�=B	{�B	v�B	m�B	dZB	^5B	YB	VB	L�B	E�B	?}B	7LB	(�B	JB	B��B�;B�B��B��B��B��BǮBŢBÖB��B��B��B�}B�wB�qB�^B�XB�LB�LB�FB�9B�-B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�oB��B��B��B��B�{B�{B��B��B��B��B��B�VB�VB�VB�bB��B��B��B��B��B��B�uB�oB�VB�DB�1B�+B�+B�+B�7B�DB�VB��B��B��B��B��B��B��B��B��B��B�PBz�B}�B��B�B�!B�!B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�!B�jB��B��B��B��B��BBBĜBÖBŢB��B��B��B��B��B��B��B��B��B��B��B�B�B�
B��B�B�HB�B��B	
=B	
=B	B	PB	bB	VB	PB	hB	uB	uB	{B	{B	{B	�B	�B	�B	�B	�B	�B	�B	�B	!�B	!�B	!�B	"�B	$�B	&�B	)�B	-B	1'B	33B	6FB	7LB	9XB	9XB	:^B	:^B	;dB	<jB	?}B	B�B	C�B	C�B	D�B	I�B	J�B	N�B	R�B	W
B	W
B	^5B	cTB	dZB	e`B	e`B	ffB	iyB	m�B	s�B	w�B	x�B	|�B	�B	�B	�B	�B	�B	�B	�B	�B	�1B	�VB	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�FB	�qB	��B	ÖB	��B	�}B	��B	ŢB	ŢB	ÖB	ŢB	ƨB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�)B	�/B	�/B	�;B	�/B	�)B	�/B	�;B	�TB	�TB	�NB	�NB	�ZB	�`B	�fB	�fB	�mB	�mB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
  B
  B
B
B
B
B
B
B
B
B
B
%B
+B
+B
+B
+B
+B
+B
+B
+B
+B
+B
1B
1B
	7B
	7B
	7B

=B

=B

=B

=B

=B

=B

=B

=B

=B
DB
DB

=B
DB

=B
DB
DB
JB
JB
JB
JB
JB
JB
JB
JB
PB
PB
PB
PB
PB
VB
VB
\B
hB
hB
hB
hB
hB
hB
hB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
0;1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B�7B�%B�B�B�B�B�B�B�B�B�B�B�B� B~�B{�By�Bw�Bu�B�+B�JB��B�-B�yB�B�B�B��B��B��B��B\BL�BC�B=qB:^B33B0!B-B0!B5?B2-B.B �B{B\BDB+BBB��B�sBB��B��B�1Bz�Bt�Bo�BhsBcTB]/BQ�BB�B+B�BVB
��B
�B
�mB
ɺB
��B
v�B
B�B
 �B
B	�;B	��B	�^B	�=B	{�B	v�B	m�B	dZB	^5B	YB	VB	L�B	E�B	?}B	7LB	(�B	JB	B��B�;B�B��B��B��B��BǮBŢBÖB��B��B��B�}B�wB�qB�^B�XB�LB�LB�FB�9B�-B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�oB��B��B��B��B�{B�{B��B��B��B��B��B�VB�VB�VB�bB��B��B��B��B��B��B�uB�oB�VB�DB�1B�+B�+B�+B�7B�DB�VB��B��B��B��B��B��B��B��B��B��B�PBz�B}�B��B�B�!B�!B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�!B�jB��B��B��B��B��BBBĜBÖBŢB��B��B��B��B��B��B��B��B��B��B��B�B�B�
B��B�B�HB�B��B	
=B	
=B	B	PB	bB	VB	PB	hB	uB	uB	{B	{B	{B	�B	�B	�B	�B	�B	�B	�B	�B	!�B	!�B	!�B	"�B	$�B	&�B	)�B	-B	1'B	33B	6FB	7LB	9XB	9XB	:^B	:^B	;dB	<jB	?}B	B�B	C�B	C�B	D�B	I�B	J�B	N�B	R�B	W
B	W
B	^5B	cTB	dZB	e`B	e`B	ffB	iyB	m�B	s�B	w�B	x�B	|�B	�B	�B	�B	�B	�B	�B	�B	�B	�1B	�VB	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�FB	�qB	��B	ÖB	��B	�}B	��B	ŢB	ŢB	ÖB	ŢB	ƨB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�)B	�/B	�/B	�;B	�/B	�)B	�/B	�;B	�TB	�TB	�NB	�NB	�ZB	�`B	�fB	�fB	�mB	�mB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
  B
  B
B
B
B
B
B
B
B
B
B
%B
+B
+B
+B
+B
+B
+B
+B
+B
+B
+B
1B
1B
	7B
	7B
	7B

=B

=B

=B

=B

=B

=B

=B

=B

=B
DB
DB

=B
DB

=B
DB
DB
JB
JB
JB
JB
JB
JB
JB
JB
PB
PB
PB
PB
PB
VB
VB
\B
hB
hB
hB
hB
hB
hB
hB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
0;1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.33 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181024140820                              AO  ARCAADJP                                                                    20181024140820    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181024140820  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181024140820  QCF$                G�O�G�O�G�O�0               