CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:05:59Z creation      
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
_FillValue                 �  A4   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C(   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  T�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \h   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ^\   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  f    TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  h   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  o�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  w�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  y�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �T   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �H   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �<   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �<   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �<   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �<   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �h   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �l   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �p   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �t   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �x   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20181005190559  20181005190559  5904952 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6431                            2B  A   APEX                            7466                            062512                          846 @��i��1   @��jI��6@0�(�\�c�-1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �A   A   A   @���@�  A   A   A@  A^ffA�  A�  A�  A�  A�33A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BPffBX  B`  BhffBpffBx  B�  B���B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(�C*  C,  C.�C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C��C�  C�  C�  C�  C�  C��C��C��C�  C��3C�  C��C�  C�  C�  C��3C�  C�  C�  C�  C��C�  C�  C�  C�  C��3C�  C�  C��3C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C��C��C�  C��C�  C��C��C�  C�  C�  C�  C��3C��3C�  C��C�  C�  C�  C�  C�  C�  C�  C��3C��3C��3C��3C�  C��C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  D   D � D  D� D  Dy�D  D� D  D�fDfD� D  D� DfD�fD  D� D	  D	� D
  D
� D  D�fD  D� D  Dy�D  D� DfD� D��Dy�D  D� D��Dy�DfD�fD  D� D  Dy�D  D�fD  D� D��D� DfD� D  D� D  D� D  D� D��Dy�D��Dy�D  D� D fD � D!  D!� D"  D"�fD#  D#� D$  D$�fD%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+y�D,  D,� D-  D-� D.fD.�fD/  D/� D0  D0� D1  D1� D2  D2� D3  D3�fD4fD4� D4��D5y�D6  D6�fD7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D;��D<� D=fD=� D=��D>y�D?  D?� D?��D@� DAfDA�fDB  DB� DC  DCy�DC��DD� DE  DE� DFfDF�fDG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DPfDP� DP��DQ� DR  DRy�DS  DS� DT  DT� DU  DU� DV  DV� DWfDW�fDXfDX�fDYfDY� DY��DZ� D[fD[�fD\  D\� D]  D]� D^  D^� D_  D_� D`  D`y�Da  Da�fDb  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Djy�Dj��Dky�Dl  Dl� Dl��Dm� Dn  Dn� Dn��Do� Dp  Dp� Dq  Dqy�Dr  Dr� Ds  Ds� Dt  Dt�fDu  Duy�Dv  Dv�fDw  Dw� Dw� Dy�fD�9�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�(�@ʏ\AG�A%G�AEG�Ac�A���A���A���A���A��
Aң�A��A��BQ�B	Q�BQ�BQ�B!Q�B)Q�B1Q�B9Q�BAQ�BIQ�BQ�RBYQ�BaQ�Bi�RBq�RByQ�B���B�u�B���B���B���B���B��)B���B���B���B���B���B���B���B���B���B���BĨ�BȨ�B̨�BШ�BԨ�Bب�Bܨ�B��B��B��B��B��B���B���B���C T{CT{CT{CT{CT{C
T{CT{CT{CT{CT{CT{CT{CT{CT{CT{CT{C T{C"T{C$T{C&T{C(nC*T{C,T{C.nC0T{C2T{C4T{C6T{C8T{C:T{C<T{C>T{C@T{CBT{CDT{CFT{CHT{CJT{CLT{CNT{CPT{CRT{CTT{CVT{CXT{CZT{C\T{C^T{C`T{CbT{CdT{CfT{ChT{CjT{ClT{CnT{CpT{CrT{CtT{CvT{CxT{CzT{C|T{C~T{C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�7
C�*=C�*=C�*=C�*=C�*=C�*=C�7
C�*=C�*=C�*=C�7
C�*=C�*=C�*=C�*=C�*=C�7
C�7
C�7
C�*=C�pC�*=C�7
C�*=C�*=C�*=C�pC�*=C�*=C�*=C�*=C�7
C�*=C�*=C�*=C�*=C�pC�*=C�*=C�pC�pC�pC�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�pC�*=C�*=C�*=C�*=C�*=C�*=C�7
C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�pC�*=C�7
C�7
C�*=C�7
C�*=C�7
C�7
C�*=C�*=C�*=C�*=C�pC�pC�*=C�7
C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�pC�pC�pC�pC�*=C�7
C�*=C�*=C�*=C�*=C�*=C�pC�pC�*=C�*=C�*=C�*=D D �DD�DD��DD�DD��D�D�DD�D�D��DD�D	D	�D
D
�DD��DD�DD��DD�D�D�D�D��DD�D�D��D�D��DD�DD��DD��DD�D�D�D�D�DD�DD�DD�D�D��D�D��DD�D �D �D!D!�D"D"��D#D#�D$D$��D%D%�D&D&�D'D'�D(D(�D)D)�D*D*�D+D+��D,D,�D-D-�D.�D.��D/D/�D0D0�D1D1�D2D2�D3D3��D4�D4�D5�D5��D6D6��D7D7�D8D8�D9D9�D:D:�D;D;�D<�D<�D=�D=�D>�D>��D?D?�D@�D@�DA�DA��DBDB�DCDC��DD�DD�DEDE�DF�DF��DGDG�DHDH�DIDI�DJDJ�DKDK�DLDL�DMDM�DNDN�DODO�DP�DP�DQ�DQ�DRDR��DSDS�DTDT�DUDU�DVDV�DW�DW��DX�DX��DY�DY�DZ�DZ�D[�D[��D\D\�D]D]�D^D^�D_D_�D`D`��DaDa��DbDb�DcDc�DdDd�DeDe�DfDf�DgDg�DhDh�DiDi�DjDj��Dk�Dk��DlDl�Dm�Dm�DnDn�Do�Do�DpDp�DqDq��DrDr�DsDs�DtDt��DuDu��DvDv��DwDw�Dw�Dy��D�D{11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�-A�/A���AǶFAǡ�A�~�A�`BA�C�A�&�A�%A��AƼjAƬAƗ�AƁA�ffA�^5A�XA�9XA�"�A�"�A�=qA�M�A�^5A�dZA�n�A�x�AƁAƅA�p�A�=qA�+A�/A�1'A�9XA�K�A�VA�ffA�r�A�~�AƏ\AƟ�Aƣ�AƩ�AƮAư!AƲ-AƮAƙ�AƋDA�t�A�/A��AËDA�r�A�  A�\)A���A�C�A�JA��;A�x�A��
A��A�/A���A��!A�A���A�jA�1A���A��
A��A�x�A���A���A� �A�?}A���A���A�$�A�A�l�A���A��wA��A�?}A�A�XA��#A�1'A��\A�C�A�Q�A��9A�v�A��A���A��FA�VA�bA��A�A���A�1'A|��AxĜAv�/At�yArZAoG�Ak�-AhQ�Ad��AcO�Ab�yA_�A]dZA\=qAZ�\AW��AU`BAS��AR�AN(�AGS�AA�PA>~�A=��A<�9A:z�A8^5A7oA4M�A2��A21A1"�A0�DA/�wA/dZA/&�A.z�A-�^A-K�A,�A+/A(��A(M�A'�TA'"�A&=qA%�A$��A#�A"r�A!��A!S�A��A�yA{AK�A+A�-Av�AƨA~�A�7A�+A�A��A��A�/A��A��Ap�A
�jA
�A	p�A$�A&�A�Al�A-A��A%A�/A��A�A ��A 9X@�$�@��@�ff@���@�G�@��w@��@��/@��u@�w@�\)@�C�@�?}@��@��-@�%@�Z@�R@�`B@�r�@�K�@�?}@� �@��H@�h@��
@߅@ܴ9@��
@�ƨ@۾w@�33@ّh@؃@׮@��@�ȴ@�$�@թ�@�p�@���@��@�~�@�E�@�M�@Ѳ-@щ7@��@�r�@��
@��@�ff@�5?@��#@�x�@���@�1@��y@�M�@���@��@���@��;@ǶF@�@��@�J@�J@�-@ř�@��@�@Ł@�S�@�ȴ@§�@öF@�^5@�j@��P@�l�@�
=@�ȴ@���@�ff@�K�@��
@���@�&�@�`B@�hs@��D@�K�@�V@�{@�$�@��@���@�7L@�z�@�bN@��@��!@�;d@���@��+@��@�G�@��@��;@�n�@���@��^@��@��@�O�@�$�@�M�@�v�@�E�@��#@���@��7@��@�b@��y@���@��j@�"�@���@�~�@�=q@��@�5?@���@�7L@�j@�j@��D@�1'@��;@��F@�K�@��@���@�l�@�dZ@�dZ@�
=@�~�@��-@��7@�G�@�?}@�&�@���@��u@�bN@�bN@�z�@�Z@�(�@� �@�A�@�(�@��
@�t�@�33@��@���@�n�@��-@��7@�hs@��@�b@��m@��
@�l�@��!@���@���@���@�ff@�J@��7@�7L@��j@�(�@�b@��
@���@��@�t�@�;d@��@���@���@�E�@��@���@��@�`B@�O�@�V@���@�b@��@��;@��
@���@�33@�@��y@�ȴ@��!@�v�@�E�@��T@��^@���@�hs@�?}@���@�Ĝ@�j@�1@��
@��w@���@�l�@�;d@�+@�+@�+@��@�~�@�5?@��^@���@��7@�hs@�V@���@�Ĝ@��@��u@� �@�ƨ@��P@�dZ@�+@�
=@�ȴ@�^5@�$�@���@���@���@�O�@��@��@�A�@��;@��@���@���@��@���@���@�~�@�M�@�J@��T@�@���@��7@�G�@��`@���@���@��@���@�Q�@�1@��w@��w@��F@���@�|�@�S�@��y@��y@���@�-@���@�hs@�&�@��@�A�@�  @��@�C�@���@��$@�!�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�-A�/A���AǶFAǡ�A�~�A�`BA�C�A�&�A�%A��AƼjAƬAƗ�AƁA�ffA�^5A�XA�9XA�"�A�"�A�=qA�M�A�^5A�dZA�n�A�x�AƁAƅA�p�A�=qA�+A�/A�1'A�9XA�K�A�VA�ffA�r�A�~�AƏ\AƟ�Aƣ�AƩ�AƮAư!AƲ-AƮAƙ�AƋDA�t�A�/A��AËDA�r�A�  A�\)A���A�C�A�JA��;A�x�A��
A��A�/A���A��!A�A���A�jA�1A���A��
A��A�x�A���A���A� �A�?}A���A���A�$�A�A�l�A���A��wA��A�?}A�A�XA��#A�1'A��\A�C�A�Q�A��9A�v�A��A���A��FA�VA�bA��A�A���A�1'A|��AxĜAv�/At�yArZAoG�Ak�-AhQ�Ad��AcO�Ab�yA_�A]dZA\=qAZ�\AW��AU`BAS��AR�AN(�AGS�AA�PA>~�A=��A<�9A:z�A8^5A7oA4M�A2��A21A1"�A0�DA/�wA/dZA/&�A.z�A-�^A-K�A,�A+/A(��A(M�A'�TA'"�A&=qA%�A$��A#�A"r�A!��A!S�A��A�yA{AK�A+A�-Av�AƨA~�A�7A�+A�A��A��A�/A��A��Ap�A
�jA
�A	p�A$�A&�A�Al�A-A��A%A�/A��A�A ��A 9X@�$�@��@�ff@���@�G�@��w@��@��/@��u@�w@�\)@�C�@�?}@��@��-@�%@�Z@�R@�`B@�r�@�K�@�?}@� �@��H@�h@��
@߅@ܴ9@��
@�ƨ@۾w@�33@ّh@؃@׮@��@�ȴ@�$�@թ�@�p�@���@��@�~�@�E�@�M�@Ѳ-@щ7@��@�r�@��
@��@�ff@�5?@��#@�x�@���@�1@��y@�M�@���@��@���@��;@ǶF@�@��@�J@�J@�-@ř�@��@�@Ł@�S�@�ȴ@§�@öF@�^5@�j@��P@�l�@�
=@�ȴ@���@�ff@�K�@��
@���@�&�@�`B@�hs@��D@�K�@�V@�{@�$�@��@���@�7L@�z�@�bN@��@��!@�;d@���@��+@��@�G�@��@��;@�n�@���@��^@��@��@�O�@�$�@�M�@�v�@�E�@��#@���@��7@��@�b@��y@���@��j@�"�@���@�~�@�=q@��@�5?@���@�7L@�j@�j@��D@�1'@��;@��F@�K�@��@���@�l�@�dZ@�dZ@�
=@�~�@��-@��7@�G�@�?}@�&�@���@��u@�bN@�bN@�z�@�Z@�(�@� �@�A�@�(�@��
@�t�@�33@��@���@�n�@��-@��7@�hs@��@�b@��m@��
@�l�@��!@���@���@���@�ff@�J@��7@�7L@��j@�(�@�b@��
@���@��@�t�@�;d@��@���@���@�E�@��@���@��@�`B@�O�@�V@���@�b@��@��;@��
@���@�33@�@��y@�ȴ@��!@�v�@�E�@��T@��^@���@�hs@�?}@���@�Ĝ@�j@�1@��
@��w@���@�l�@�;d@�+@�+@�+@��@�~�@�5?@��^@���@��7@�hs@�V@���@�Ĝ@��@��u@� �@�ƨ@��P@�dZ@�+@�
=@�ȴ@�^5@�$�@���@���@���@�O�@��@��@�A�@��;@��@���@���@��@���@���@�~�@�M�@�J@��T@�@���@��7@�G�@��`@���@���@��@���@�Q�@�1@��w@��w@��F@���@�|�@�S�@��y@��y@���@�-@���@�hs@�&�@��@�A�@�  @��@�C�@���@��$@�!�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B1'B1'B/B0!B1'B49B7LB:^B?}BF�BL�BYBdZBl�By�B�+B�PB��B��B�3BĜB��B�)B�ZB�mB�B�B�B��B	B	(�B	I�B	O�B	S�B	YB	]/B	_;B	cTB	ffB	iyB	m�B	q�B	s�B	v�B	x�B	{�B	}�B	� B	� B	}�B	}�B	��B
��B
�BH�Bm�Bv�B�B�uB��B��B�B��BB
=B
=BVB{B�B�B�B�B�BoB��B�NB�BB�5B�B��BȴBÖB�jB�?B�!B��B��B�JB}�Bs�BS�B�BPB
�B
�/B
B
�RB
�}B
�B
��B
�7B
W
B
�B
uB
%B	��B	�sB	��B	�jB	��B	��B	�PB	r�B	[#B	Q�B	VB	Q�B	>wB	1'B	)�B	 �B	bB	B��B�B�
B�qB�!B��B��B��B�{B�PB�PB�oB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�9B�XB�^B�^B�jB��B�wB�XB�jB�^B�FB�9B�3B�-B�?B�RB�XB�jB��B��B�wB��B�}B��BÖBB�}B��BÖB��B��BBÖBB�wB�}B�}B�wB��B��B�}B�}B�}B�wB�qB�jB�wB��B�wB�}B�wB�}BBŢB��B��B��B��B��B��B��B�/B�/B�`B�sB�yB�B�B�B��B��B��B��B�B��B��B��B��B��B��B	B	B	%B	1B	
=B	JB	\B	uB	�B	!�B	&�B	(�B	,B	.B	1'B	2-B	8RB	;dB	<jB	<jB	<jB	>wB	@�B	A�B	C�B	E�B	I�B	R�B	]/B	bNB	]/B	aHB	iyB	q�B	p�B	jB	hsB	k�B	n�B	q�B	u�B	{�B	�1B	�JB	�hB	��B	��B	��B	��B	��B	�uB	��B	��B	��B	��B	�\B	�bB	�uB	��B	��B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�XB	�jB	�jB	��B	ƨB	ǮB	ǮB	ȴB	ƨB	B	��B	�wB	�wB	�qB	�qB	�qB	�}B	B	��B	��B	B	ÖB	ƨB	��B	��B	��B	��B	��B	�B	�
B	�
B	�B	�B	�B	�
B	�
B	�B	�B	�B	�B	�B	�#B	�)B	�/B	�5B	�;B	�HB	�NB	�TB	�TB	�TB	�TB	�ZB	�`B	�`B	�`B	�fB	�mB	�fB	�fB	�mB	�mB	�mB	�sB	�yB	�B	�B	�B	�B	�yB	�sB	�sB	�mB	�sB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
+B
1B

=B

=B
JB
PB
VB
VB
VB
VB
VB
\B
\B
\B
\B
bB
bB
bB
bB
bB
hB
hB
hB
hB
hB
hB
oB
hB
hB
hB
hB
hB
hB
oB
uB
oB
oB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
,B
 �B
3322222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222   B1'B1'B/B0!B1'B49B7LB:^B?}BF�BL�BYBdZBl�By�B�+B�PB��B��B�3BĜB��B�)B�ZB�mB�B�B�B��B	B	(�B	I�B	O�B	S�B	YB	]/B	_;B	cTB	ffB	iyB	m�B	q�B	s�B	v�B	x�B	{�B	}�B	� B	� B	}�B	}�B	��B
��B
�BH�Bm�Bv�B�B�uB��B��B�B��BB
=B
=BVB{B�B�B�B�B�BoB��B�NB�BB�5B�B��BȴBÖB�jB�?B�!B��B��B�JB}�Bs�BS�B�BPB
�B
�/B
B
�RB
�}B
�B
��B
�7B
W
B
�B
uB
%B	��B	�sB	��B	�jB	��B	��B	�PB	r�B	[#B	Q�B	VB	Q�B	>wB	1'B	)�B	 �B	bB	B��B�B�
B�qB�!B��B��B��B�{B�PB�PB�oB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�9B�XB�^B�^B�jB��B�wB�XB�jB�^B�FB�9B�3B�-B�?B�RB�XB�jB��B��B�wB��B�}B��BÖBB�}B��BÖB��B��BBÖBB�wB�}B�}B�wB��B��B�}B�}B�}B�wB�qB�jB�wB��B�wB�}B�wB�}BBŢB��B��B��B��B��B��B��B�/B�/B�`B�sB�yB�B�B�B��B��B��B��B�B��B��B��B��B��B��B	B	B	%B	1B	
=B	JB	\B	uB	�B	!�B	&�B	(�B	,B	.B	1'B	2-B	8RB	;dB	<jB	<jB	<jB	>wB	@�B	A�B	C�B	E�B	I�B	R�B	]/B	bNB	]/B	aHB	iyB	q�B	p�B	jB	hsB	k�B	n�B	q�B	u�B	{�B	�1B	�JB	�hB	��B	��B	��B	��B	��B	�uB	��B	��B	��B	��B	�\B	�bB	�uB	��B	��B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�XB	�jB	�jB	��B	ƨB	ǮB	ǮB	ȴB	ƨB	B	��B	�wB	�wB	�qB	�qB	�qB	�}B	B	��B	��B	B	ÖB	ƨB	��B	��B	��B	��B	��B	�B	�
B	�
B	�B	�B	�B	�
B	�
B	�B	�B	�B	�B	�B	�#B	�)B	�/B	�5B	�;B	�HB	�NB	�TB	�TB	�TB	�TB	�ZB	�`B	�`B	�`B	�fB	�mB	�fB	�fB	�mB	�mB	�mB	�sB	�yB	�B	�B	�B	�B	�yB	�sB	�sB	�mB	�sB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
+B
1B

=B

=B
JB
PB
VB
VB
VB
VB
VB
\B
\B
\B
\B
bB
bB
bB
bB
bB
hB
hB
hB
hB
hB
hB
oB
hB
hB
hB
hB
hB
hB
oB
uB
oB
oB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
,B
 �B
3322222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.33 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005190559                              AO  ARCAADJP                                                                    20181005190559    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005190559  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005190559  QCF$                G�O�G�O�G�O�8000            