CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:05:42Z creation      
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20181005190542  20181005190542  5904952 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6431                            2B  A   APEX                            7466                            062512                          846 @��i{�1   @��j��)@0�bM���c��E��1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �A   A   A   @�33@�  A   A   A@  A`  A�  A�  A�  A�33A�33A�  A�  A�33B   BffB  B  B   B(  B/��B7��B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B���B���B���B�  B�  B�  B�  B�  B�  B�  B�33B�  B���C  C  C  C  C
  C  C  C  C  C  C  C�fC�fC  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH�CJ�CL  CN  CP  CR  CT  CV  CX  CZ  C[�fC^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C��C�  C��3C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C��C��C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C��C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C��C��C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C��3C�  C��3D   D � DfD� D  D� D  D� D  D� D  D� D  D� D  D�fD  Dy�D	  D	� D	��D
� D  D� D  D� D  D� D  D� D  D� D  D� DfDy�D  D� D  D� D��Dy�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D��Dy�D  D� D��D y�D!  D!� D!��D"� D#  D#� D$  D$� D$��D%� D&  D&� D'  D'� D(  D(� D(��D)� D*  D*y�D*��D+� D,  D,�fD-  D-� D.  D.� D/  D/y�D0  D0� D1  D1� D2  D2� D3  D3� D3��D4� D5  D5� D6  D6� D7fD7�fD8fD8� D9  D9� D9��D:y�D:��D;� D;��D<y�D<��D=� D>  D>� D?fD?� D@  D@�fDA  DA� DBfDB� DB��DC� DD  DDy�DE  DE� DE��DFy�DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DKy�DL  DL� DM  DM� DN  DN� DO  DO�fDP  DPy�DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU�fDVfDV� DW  DW�fDXfDX�fDY  DY� DY��DZ� D[  D[�fD\fD\� D]  D]y�D^  D^�fD_  D_�fD`  D`y�D`��Day�Db  Db� Db��Dc� Dd  Dd� De  De�fDf  Dfy�Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm�fDn  Dn� DofDo�fDp  Dpy�Dq  Dq� Dr  Dry�Dr��Ds� DtfDt�fDu  Du� Dv  Dv� Dw  Dwy�DwٚDy�D�E�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�@ʏ\AG�A%G�AEG�AeG�A���A���A���A��
A��
Aң�A��A��
BQ�B	�RBQ�BQ�B!Q�B)Q�B0�B8�BAQ�BIQ�BQQ�BYQ�BaQ�BiQ�BqQ�ByQ�B���B���B���B���B���B���B���B���B���B���B���B�u�B���B���B���B���B���BĨ�BȨ�B̨�B�u�B�u�B�u�Bܨ�B��B��B��B��B��B���B��)B���C :�CT{CT{CT{CT{C
T{CT{CT{CT{CT{CT{CT{C:�C:�CT{CT{C T{C"T{C$T{C&T{C(T{C*T{C,T{C.T{C0T{C2T{C4T{C6T{C8T{C:T{C<T{C>T{C@T{CBT{CDT{CFT{CHnCJnCLT{CNT{CPT{CRT{CTT{CVT{CXT{CZT{C\:�C^T{C`T{CbT{CdT{CfT{ChT{CjT{ClT{CnT{CpT{CrT{CtT{CvT{CxT{CzT{C|T{C~T{C�*=C�*=C�*=C�7
C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�pC�pC�*=C�*=C�7
C�*=C�pC�*=C�*=C�*=C�*=C�*=C�*=C�pC�*=C�*=C�*=C�7
C�7
C�*=C�*=C�*=C�*=C�*=C�7
C�*=C�*=C�*=C�*=C�pC�pC�*=C�*=C�*=C�*=C�*=C�*=C�7
C�7
C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�pC�pC�*=C�*=C�7
C�*=C�*=C�*=C�7
C�7
C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�pC�*=C�7
C�7
C�*=C�*=C�*=C�*=C�*=C�7
C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�pC�*=C�*=C�*=C�*=C�pC�*=C�pD D �D�D�DD�DD�DD�DD�DD�DD��DD��D	D	�D
�D
�DD�DD�DD�DD�DD�DD�D�D��DD�DD�D�D��DD�DD�DD�DD�DD�DD�DD�DD��DD�D�D��DD�D �D ��D!D!�D"�D"�D#D#�D$D$�D%�D%�D&D&�D'D'�D(D(�D)�D)�D*D*��D+�D+�D,D,��D-D-�D.D.�D/D/��D0D0�D1D1�D2D2�D3D3�D4�D4�D5D5�D6D6�D7�D7��D8�D8�D9D9�D:�D:��D;�D;�D<�D<��D=�D=�D>D>�D?�D?�D@D@��DADA�DB�DB�DC�DC�DDDD��DEDE�DF�DF��DGDG�DHDH�DIDI�DJDJ�DKDK��DLDL�DMDM�DNDN�DODO��DPDP��DQDQ�DRDR�DSDS�DTDT�DUDU��DV�DV�DWDW��DX�DX��DYDY�DZ�DZ�D[D[��D\�D\�D]D]��D^D^��D_D_��D`D`��Da�Da��DbDb�Dc�Dc�DdDd�DeDe��DfDf��DgDg�DhDh�DiDi�DjDj�DkDk�DlDl�DmDm��DnDn�Do�Do��DpDp��DqDq�DrDr��Ds�Ds�Dt�Dt��DuDu�DvDv�DwDw��Dw�Dy�3D�PR11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Aщ7AхAыDAѕ�Aѕ�AѓuAї�Aћ�Aћ�Aљ�AѓuAћ�Aџ�Aѣ�Aѥ�Aѩ�Aѩ�Aѡ�AыDA�x�A�ffA�O�A�1'A� �A�{A���A��A��/A���Aд9AЁA�hsA�K�A�I�A�?}A�&�A�  A���A�ZA�A�JA��#Aˉ7A�XA�jAǧ�A�7LA�%A���A���Aå�A�l�A²-A��A��RA���A��A��uA��DA��#A�^5A���A��A�\)A��\A���A�+A���A�7LA�XA�O�A���A���A�{A���A��PA�G�A��+A��9A�ZA��TA���A���A���A�  A��PA���A���A��jA��jA���A��hA�l�A��mA��`A���A���A��A�|�A��yA��+A��A�v�A���A��A��At�A}�;A|{Ax�9Au�
As�Ao��AlȴAh�yAc�wA_��AZ�jAX�+AR1'AOl�AN��AN�RANJAK`BAE�wAB1'A?��A>ȴA=�A=C�A<�A;��A9�A7��A6JA4�A2�/A2A�A1��A0bNA/&�A.(�A-�-A,��A+�A)�hA'��A'��A&��A$$�A"�HA"�\A!�A!S�A �/A JA�!A�^AdZAG�A�AƨA33A�!AA�A�A"�A��A�A|�AhsA�jA�A
=Ar�Ax�A��A1A&�A��A �A�^A��A
�AjA7LA��A��A33A�A�!A^5A�A$�A z�A V@�~�@�&�@��u@�9X@�b@�C�@���@��
@��+@���@��@�A�@�33@�/@��@�r�@�n�@�X@�V@�@��@��@��@�ff@��@�?}@� �@�33@�v�@�@���@�Z@��@ߥ�@�|�@�o@��@�/@ܓu@�5?@�p�@ؼj@�A�@�b@�t�@�+@�M�@�1@�{@щ7@�V@д9@�bN@���@ϥ�@�\)@�C�@�C�@�33@��@�o@��@�"�@�;d@�+@��@θR@���@���@���@���@̣�@�ƨ@�
=@ʸR@�M�@�-@�hs@�I�@�\)@�o@�ȴ@�V@őh@�j@�l�@�@�E�@��@��-@�p�@�%@�Ĝ@�j@���@�\)@���@�n�@��T@���@��@��m@�C�@�E�@���@�X@�V@���@��@� �@���@�@��@���@�{@�O�@��@�%@���@��9@��D@��@��@��w@���@�;d@��H@��\@��h@�?}@�z�@��@�|�@�o@���@�V@�M�@��@���@�@���@�7L@��j@�1'@��
@�dZ@��@��y@���@��R@���@�ff@�V@�=q@��@��7@�G�@�&�@���@��@�Q�@���@�"�@��y@���@�ff@�5?@���@�p�@�`B@�`B@�O�@�X@���@�bN@�A�@�(�@�b@���@��@���@�^5@��@��h@�O�@��@��@��@��j@�1@��;@��F@�ƨ@��w@�t�@�t�@�K�@�+@��@��\@��\@�^5@�@���@�p�@�7L@��@���@�Z@�1'@�9X@� �@� �@�b@��w@�33@��+@��@���@��@�G�@��/@�1@�S�@��@���@���@�V@�=q@��@���@��T@���@���@��u@�A�@��;@�l�@��@��@���@��!@�n�@�$�@���@��@��@� �@���@�t�@�+@�o@�
=@��y@��+@�M�@��@�@��7@�?}@��/@���@�r�@��@���@��m@��
@���@��@�@���@�^5@��@���@��-@���@�G�@���@���@��D@�bN@��@�|�@�o@���@���@��!@��\@�~�@�v�@�=q@��T@���@��@�hs@�`B@�?}@��@���@vYK11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   Aщ7AхAыDAѕ�Aѕ�AѓuAї�Aћ�Aћ�Aљ�AѓuAћ�Aџ�Aѣ�Aѥ�Aѩ�Aѩ�Aѡ�AыDA�x�A�ffA�O�A�1'A� �A�{A���A��A��/A���Aд9AЁA�hsA�K�A�I�A�?}A�&�A�  A���A�ZA�A�JA��#Aˉ7A�XA�jAǧ�A�7LA�%A���A���Aå�A�l�A²-A��A��RA���A��A��uA��DA��#A�^5A���A��A�\)A��\A���A�+A���A�7LA�XA�O�A���A���A�{A���A��PA�G�A��+A��9A�ZA��TA���A���A���A�  A��PA���A���A��jA��jA���A��hA�l�A��mA��`A���A���A��A�|�A��yA��+A��A�v�A���A��A��At�A}�;A|{Ax�9Au�
As�Ao��AlȴAh�yAc�wA_��AZ�jAX�+AR1'AOl�AN��AN�RANJAK`BAE�wAB1'A?��A>ȴA=�A=C�A<�A;��A9�A7��A6JA4�A2�/A2A�A1��A0bNA/&�A.(�A-�-A,��A+�A)�hA'��A'��A&��A$$�A"�HA"�\A!�A!S�A �/A JA�!A�^AdZAG�A�AƨA33A�!AA�A�A"�A��A�A|�AhsA�jA�A
=Ar�Ax�A��A1A&�A��A �A�^A��A
�AjA7LA��A��A33A�A�!A^5A�A$�A z�A V@�~�@�&�@��u@�9X@�b@�C�@���@��
@��+@���@��@�A�@�33@�/@��@�r�@�n�@�X@�V@�@��@��@��@�ff@��@�?}@� �@�33@�v�@�@���@�Z@��@ߥ�@�|�@�o@��@�/@ܓu@�5?@�p�@ؼj@�A�@�b@�t�@�+@�M�@�1@�{@щ7@�V@д9@�bN@���@ϥ�@�\)@�C�@�C�@�33@��@�o@��@�"�@�;d@�+@��@θR@���@���@���@���@̣�@�ƨ@�
=@ʸR@�M�@�-@�hs@�I�@�\)@�o@�ȴ@�V@őh@�j@�l�@�@�E�@��@��-@�p�@�%@�Ĝ@�j@���@�\)@���@�n�@��T@���@��@��m@�C�@�E�@���@�X@�V@���@��@� �@���@�@��@���@�{@�O�@��@�%@���@��9@��D@��@��@��w@���@�;d@��H@��\@��h@�?}@�z�@��@�|�@�o@���@�V@�M�@��@���@�@���@�7L@��j@�1'@��
@�dZ@��@��y@���@��R@���@�ff@�V@�=q@��@��7@�G�@�&�@���@��@�Q�@���@�"�@��y@���@�ff@�5?@���@�p�@�`B@�`B@�O�@�X@���@�bN@�A�@�(�@�b@���@��@���@�^5@��@��h@�O�@��@��@��@��j@�1@��;@��F@�ƨ@��w@�t�@�t�@�K�@�+@��@��\@��\@�^5@�@���@�p�@�7L@��@���@�Z@�1'@�9X@� �@� �@�b@��w@�33@��+@��@���@��@�G�@��/@�1@�S�@��@���@���@�V@�=q@��@���@��T@���@���@��u@�A�@��;@�l�@��@��@���@��!@�n�@�$�@���@��@��@� �@���@�t�@�+@�o@�
=@��y@��+@�M�@��@�@��7@�?}@��/@���@�r�@��@���@��m@��
@���@��@�@���@�^5@��@���@��-@���@�G�@���@���@��D@�bN@��@�|�@�o@���@���@��!@��\@�~�@�v�@�=q@��T@���@��@�hs@�`B@�?}@��@���@vYK11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B2-B2-B2-B2-B2-B2-B2-B2-B2-B2-B33B49B6FB:^B?}BK�BP�BP�BL�BJ�BK�BM�BP�BS�BT�BW
BXBZB\)B\)BW
BVBVBW
BbNBp�B|�B~�B�B��B�wB��B��BĜB�B��B�B)�B33B9XB;dB;dB>wBE�BI�BO�B]/BbNBe`BiyBl�Bn�Bo�Bt�Bt�Br�Bn�BffBe`B`BB_;B^5B\)BXBVBR�BK�BF�B>wB1'B�BuBB�`B�B��B��B��B��B�=Bx�BgmBI�B�B
��B
�#B
ƨB
�^B
�B
��B
��B
�{B
�%B
hsB
>wB	�B	��B	ŢB	�jB	�B	��B	��B	�VB	�B	o�B	XB	A�B	&�B	�B��B�B�B�B�fB�)B��BĜB�wB�wB�qB�qB�jB�XB�LB�?B�3B�-B�-B�'B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B��B��B��B��B��B�B�FB�XB�XB�}BB�}B�dB�qB�jB�}B��B��B�B�5B�BB�;B�BB�;B�;B�;B�HB�ZB�fB�yB�yB�sB�mB�fB�yB�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B	  B	B	B	B	1B	PB	oB	{B	oB	oB	{B	�B	�B	�B	�B	�B	�B	�B	�B	�B	 �B	!�B	$�B	'�B	)�B	+B	(�B	'�B	+B	-B	/B	33B	49B	49B	6FB	;dB	;dB	:^B	;dB	@�B	D�B	F�B	G�B	K�B	M�B	O�B	T�B	W
B	XB	]/B	e`B	gmB	iyB	k�B	o�B	p�B	t�B	v�B	x�B	y�B	z�B	{�B	~�B	� B	�B	�B	�B	�B	�B	�B	�+B	�7B	�7B	�7B	�7B	�7B	�=B	�DB	�JB	�JB	�VB	�\B	�bB	�hB	�oB	�oB	�hB	�hB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�!B	�'B	�-B	�3B	�?B	�FB	�LB	�XB	�dB	�jB	�qB	�jB	�jB	�}B	�}B	��B	��B	B	B	B	B	ĜB	ȴB	ȴB	ǮB	ǮB	ǮB	ǮB	ǮB	ƨB	ƨB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�#B	�)B	�/B	�5B	�/B	�5B	�BB	�NB	�TB	�TB	�ZB	�fB	�`B	�`B	�fB	�`B	�`B	�fB	�mB	�mB	�sB	�mB	�sB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
%B
1B
1B
1B
1B
1B
1B

=B

=B

=B
DB
DB
JB
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
\B
bB
hB
hB
hB
hB
oB
oB
oB
uB
�B
�B
�B
�B
'�22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222   B2-B2-B2-B2-B2-B2-B2-B2-B2-B2-B33B49B6FB:^B?}BK�BP�BP�BL�BJ�BK�BM�BP�BS�BT�BW
BXBZB\)B\)BW
BVBVBW
BbNBp�B|�B~�B�B��B�wB��B��BĜB�B��B�B)�B33B9XB;dB;dB>wBE�BI�BO�B]/BbNBe`BiyBl�Bn�Bo�Bt�Bt�Br�Bn�BffBe`B`BB_;B^5B\)BXBVBR�BK�BF�B>wB1'B�BuBB�`B�B��B��B��B��B�=Bx�BgmBI�B�B
��B
�#B
ƨB
�^B
�B
��B
��B
�{B
�%B
hsB
>wB	�B	��B	ŢB	�jB	�B	��B	��B	�VB	�B	o�B	XB	A�B	&�B	�B��B�B�B�B�fB�)B��BĜB�wB�wB�qB�qB�jB�XB�LB�?B�3B�-B�-B�'B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B��B��B��B��B��B�B�FB�XB�XB�}BB�}B�dB�qB�jB�}B��B��B�B�5B�BB�;B�BB�;B�;B�;B�HB�ZB�fB�yB�yB�sB�mB�fB�yB�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B	  B	B	B	B	1B	PB	oB	{B	oB	oB	{B	�B	�B	�B	�B	�B	�B	�B	�B	�B	 �B	!�B	$�B	'�B	)�B	+B	(�B	'�B	+B	-B	/B	33B	49B	49B	6FB	;dB	;dB	:^B	;dB	@�B	D�B	F�B	G�B	K�B	M�B	O�B	T�B	W
B	XB	]/B	e`B	gmB	iyB	k�B	o�B	p�B	t�B	v�B	x�B	y�B	z�B	{�B	~�B	� B	�B	�B	�B	�B	�B	�B	�+B	�7B	�7B	�7B	�7B	�7B	�=B	�DB	�JB	�JB	�VB	�\B	�bB	�hB	�oB	�oB	�hB	�hB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�!B	�'B	�-B	�3B	�?B	�FB	�LB	�XB	�dB	�jB	�qB	�jB	�jB	�}B	�}B	��B	��B	B	B	B	B	ĜB	ȴB	ȴB	ǮB	ǮB	ǮB	ǮB	ǮB	ƨB	ƨB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�#B	�)B	�/B	�5B	�/B	�5B	�BB	�NB	�TB	�TB	�ZB	�fB	�`B	�`B	�fB	�`B	�`B	�fB	�mB	�mB	�sB	�mB	�sB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
%B
1B
1B
1B
1B
1B
1B

=B

=B

=B
DB
DB
JB
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
\B
bB
hB
hB
hB
hB
oB
oB
oB
uB
�B
�B
�B
�B
'�22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.33 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005190542                              AO  ARCAADJP                                                                    20181005190542    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005190542  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005190542  QCF$                G�O�G�O�G�O�8000            