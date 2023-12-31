CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:05:18Z creation      
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
_FillValue                    �tArgo profile    3.1 1.2 19500101000000  20181005190518  20181005190518  5904952 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               ;A   AO  6431                            2B  A   APEX                            7466                            062512                          846 @׾%,�1   @׾%�n�@1Ix����c�`A�7L1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      ;A   A   A   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(ffB0  B8  B@  BH  BP  BX  B`  Bh  BpffBxffB�  B�  B�33B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C�C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$�C&�C(�C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ�C\�C^  C`  Cb�Cd  Cf  Ch  Cj  Cl�Cn�Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C��3C�  C�  C�  C��C�  C�  C�  C�  C�  C��C�  C�  C��C��C�  C�  C�  C�  C�  C�  C��C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  D fD � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D� D	  D	� D	��D
� D  D�fD  D� D  D� D  Dy�D��D� DfD�fDfD� D  D� D  D� D  Dy�D  D�fDfD� D  D� D��D� D  D� D��Dy�D��Dy�D��Dy�D��D� D  D� D  Dy�D   D � D!  D!y�D"  D"�fD#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*y�D+  D+�fD,fD,� D-  D-� D.fD.�fD/fD/� D/��D0y�D0��D1� D2fD2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8y�D9  D9�fD:  D:y�D;  D;�fD<fD<� D=  D=y�D>  D>�fD?  D?� D@  D@y�D@��DA� DA��DB� DCfDC�fDD  DD� DEfDE�fDFfDF� DF��DGy�DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN�fDOfDO� DP  DP� DQ  DQ� DRfDR�fDS  DS� DT  DT� DU  DU� DU��DV� DW  DW�fDX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D\��D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dcy�Dc��Dd� De  De�fDf  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� DkfDk� Dl  Dl� Dm  Dm�fDnfDn� Dn��Doy�Dp  Dp� Dq  Dq� DrfDr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dv��Dwy�Dw�3Dy�=D�5D��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@���@ʏ\AG�A%G�AEG�AeG�A���A���A���A���A£�Aң�A��A��BQ�B	Q�BQ�BQ�B!Q�B)�RB1Q�B9Q�BAQ�BIQ�BQQ�BYQ�BaQ�BiQ�Bq�RBy�RB���B���B��)B���B���B��)B��)B���B���B���B���B���B���B���B���B���B��)BĨ�BȨ�B̨�BШ�BԨ�B�u�Bܨ�B��B��B��B��B��B���B���B���C T{CT{CT{CT{CnC
T{CT{CT{CT{CT{CT{CT{CT{CT{CT{CT{C T{C"T{C$nC&nC(nC*T{C,T{C.T{C0T{C2T{C4T{C6T{C8T{C:T{C<T{C>T{C@T{CBT{CDT{CFT{CHT{CJT{CLT{CNT{CPT{CRT{CTT{CVT{CXT{CZnC\nC^T{C`T{CbnCdT{CfT{ChT{CjT{ClnCnnCpT{CrT{CtT{CvT{CxT{CzT{C|T{C~T{C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�7
C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�pC�*=C�*=C�*=C�*=C�pC�*=C�*=C�*=C�7
C�*=C�*=C�*=C�*=C�*=C�7
C�*=C�*=C�7
C�7
C�*=C�*=C�*=C�*=C�*=C�*=C�7
C�*=C�*=C�pC�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�7
C�7
C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�pC�pC�*=C�*=C�*=C�*=D �D �DD�DD�DD�DD�DD�DD�DD�D�D�D	D	�D
�D
�DD��DD�DD�DD��D�D�D�D��D�D�DD�DD�DD��DD��D�D�DD�D�D�DD�D�D��D�D��D�D��D�D�DD�DD��D D �D!D!��D"D"��D#D#�D$D$�D%D%�D&D&�D'D'�D(D(�D)D)�D*D*��D+D+��D,�D,�D-D-�D.�D.��D/�D/�D0�D0��D1�D1�D2�D2�D3D3�D4D4�D5D5�D6D6�D7D7�D8D8��D9D9��D:D:��D;D;��D<�D<�D=D=��D>D>��D?D?�D@D@��DA�DA�DB�DB�DC�DC��DDDD�DE�DE��DF�DF�DG�DG��DHDH�DIDI�DJDJ�DKDK�DLDL�DMDM�DNDN��DO�DO�DPDP�DQDQ�DR�DR��DSDS�DTDT�DUDU�DV�DV�DWDW��DXDX�DYDY�DZDZ�D[D[�D\D\�D]�D]�D^D^�D_D_�D`D`�DaDa�DbDb�DcDc��Dd�Dd�DeDe��DfDf�DgDg�DhDh�DiDi�DjDj�Dk�Dk�DlDl�DmDm��Dn�Dn�Do�Do��DpDp�DqDq�Dr�Dr�DsDs�DtDt�DuDu�DvDv�Dw�Dw��Dw�RDy�\D�?�D�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�ZA�\)A�^5A�^5A�bNA�ffA�hsA�jA�=qA�$�A��`A���AټjAٶFA٬AٓuA�~�A�x�A�`BA�JA��mA��A�ƨA��A�O�A�A�|�A��`A�M�A�Aӣ�A�9XA��
A�^5Aџ�A��AмjA�~�A�%A�5?A�
=A̬A���A�|�A�dZA��yA�hsA�=qA�VA�t�A�ZA�%A�ffA��/A�C�A��uA�9XA�9XA�{A���A���A�G�A�&�A��A�?}A�bA���A�bNA�z�A�ƨA��`A�-A��A��A�ffA��A�G�A��PA���A��#A�(�A���A��9A�-A���A��PA���A���A�9XA�;dA��;A���A�-A��7A�bA���A��7A���A�VA�9XA~jA~5?A}�wA|�9A{ƨA{%Ay�AwoAtQ�Am��Ak�PAi�Ag�Ab�A`  A_\)A^n�AY��AWx�ATbNAN��AL��AI��AG�AF��AE�-ADE�AAG�A?C�A=33A;��A:A�A8n�A6�A5�A4��A1G�A0I�A0�A/�;A/�A.5?A-|�A$�A
ZA
~�A
n�A	�PAv�A��AhsA�RAE�AƨA7LA��A�
A&�A=qAbAȴA+An�AAVA33@�
=@�(�@�O�@�ff@���@�A�@��y@���@�7@�x�@��@��@�?}@��/@�b@�F@��@�%@�Q�@��@�dZ@畁@�1@�J@�/@��@�7L@㕁@�$�@�`B@��/@���@�(�@��@�(�@ۮ@�o@��@��@�r�@��`@ى7@�n�@��@�33@��@ّh@ؼj@ؼj@ش9@�33@�@��T@���@�@Ցh@�G�@�/@�j@��;@Ӯ@ӝ�@�t�@��@ҸR@�n�@љ�@ύP@��@�5?@�M�@���@���@�Z@�C�@�=q@�&�@ȃ@ǝ�@�l�@�Ĝ@�S�@�C�@�dZ@��H@�?}@��@��m@�+@��R@�v�@�hs@��@�Ĝ@�z�@�bN@�\)@�p�@��@�(�@��@�ƨ@���@��P@�\)@�"�@�^5@��^@��@�E�@��@��h@�/@���@���@��P@�|�@��y@�-@�J@��+@��@��\@��@�n�@�=q@�X@�&�@��@�V@��/@�Z@�A�@�Z@�9X@�9X@�9X@�9X@�9X@�9X@�I�@�bN@�9X@��
@��@�t�@�;d@�E�@��@��^@�p�@�/@�&�@���@���@��u@��@� �@��@�C�@�33@�+@�K�@�dZ@�S�@�\)@�K�@��@��!@��\@�ff@�V@��@�Ĝ@���@��y@��+@���@�%@���@�r�@�bN@�A�@��@�ƨ@�|�@�S�@��@���@�ȴ@��R@���@���@��\@���@�
=@�;d@�33@�S�@�;d@�
=@�l�@�K�@���@�
=@�o@��@�v�@���@�p�@�%@��9@�1'@�1@��
@�K�@�o@�~�@�E�@�$�@��@��^@�G�@��@��`@��9@� �@�1@�C�@��H@�ff@���@�O�@�7L@�hs@�G�@�G�@���@��@�1'@� �@�1@���@�\)@��@�dZ@�@�~�@�J@��-@��7@�p�@�X@�O�@�G�@�&�@��@�9X@�1@��@�(�@�1@�1@���@�;d@�C�@�;d@���@��@�x�@�7L@��9@��@���@���@�1'@���@��@�t�@�C�@�C�@�;d@�+@��@���@ol�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�ZA�\)A�^5A�^5A�bNA�ffA�hsA�jA�=qA�$�A��`A���AټjAٶFA٬AٓuA�~�A�x�A�`BA�JA��mA��A�ƨA��A�O�A�A�|�A��`A�M�A�Aӣ�A�9XA��
A�^5Aџ�A��AмjA�~�A�%A�5?A�
=A̬A���A�|�A�dZA��yA�hsA�=qA�VA�t�A�ZA�%A�ffA��/A�C�A��uA�9XA�9XA�{A���A���A�G�A�&�A��A�?}A�bA���A�bNA�z�A�ƨA��`A�-A��A��A�ffA��A�G�A��PA���A��#A�(�A���A��9A�-A���A��PA���A���A�9XA�;dA��;A���A�-A��7A�bA���A��7A���A�VA�9XA~jA~5?A}�wA|�9A{ƨA{%Ay�AwoAtQ�Am��Ak�PAi�Ag�Ab�A`  A_\)A^n�AY��AWx�ATbNAN��AL��AI��AG�AF��AE�-ADE�AAG�A?C�A=33A;��A:A�A8n�A6�A5�A4��A1G�A0I�A0�A/�;A/�A.5?A-|�A$�A
ZA
~�A
n�A	�PAv�A��AhsA�RAE�AƨA7LA��A�
A&�A=qAbAȴA+An�AAVA33@�
=@�(�@�O�@�ff@���@�A�@��y@���@�7@�x�@��@��@�?}@��/@�b@�F@��@�%@�Q�@��@�dZ@畁@�1@�J@�/@��@�7L@㕁@�$�@�`B@��/@���@�(�@��@�(�@ۮ@�o@��@��@�r�@��`@ى7@�n�@��@�33@��@ّh@ؼj@ؼj@ش9@�33@�@��T@���@�@Ցh@�G�@�/@�j@��;@Ӯ@ӝ�@�t�@��@ҸR@�n�@љ�@ύP@��@�5?@�M�@���@���@�Z@�C�@�=q@�&�@ȃ@ǝ�@�l�@�Ĝ@�S�@�C�@�dZ@��H@�?}@��@��m@�+@��R@�v�@�hs@��@�Ĝ@�z�@�bN@�\)@�p�@��@�(�@��@�ƨ@���@��P@�\)@�"�@�^5@��^@��@�E�@��@��h@�/@���@���@��P@�|�@��y@�-@�J@��+@��@��\@��@�n�@�=q@�X@�&�@��@�V@��/@�Z@�A�@�Z@�9X@�9X@�9X@�9X@�9X@�9X@�I�@�bN@�9X@��
@��@�t�@�;d@�E�@��@��^@�p�@�/@�&�@���@���@��u@��@� �@��@�C�@�33@�+@�K�@�dZ@�S�@�\)@�K�@��@��!@��\@�ff@�V@��@�Ĝ@���@��y@��+@���@�%@���@�r�@�bN@�A�@��@�ƨ@�|�@�S�@��@���@�ȴ@��R@���@���@��\@���@�
=@�;d@�33@�S�@�;d@�
=@�l�@�K�@���@�
=@�o@��@�v�@���@�p�@�%@��9@�1'@�1@��
@�K�@�o@�~�@�E�@�$�@��@��^@�G�@��@��`@��9@� �@�1@�C�@��H@�ff@���@�O�@�7L@�hs@�G�@�G�@���@��@�1'@� �@�1@���@�\)@��@�dZ@�@�~�@�J@��-@��7@�p�@�X@�O�@�G�@�&�@��@�9X@�1@��@�(�@�1@�1@���@�;d@�C�@�;d@���@��@�x�@�7L@��9@��@���@���@�1'@���@��@�t�@�C�@�C�@�;d@�+@��@���@ol�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
H�B
H�B
H�B
I�B
H�B
H�B
H�B
H�B
C�B
@�B
:^B
9XB
9XB
9XB
:^B
:^B
:^B
9XB
8RB
?}B
E�B
[#B
q�B
`BB
ffB
gmB
k�B
m�B
k�B
jB
k�B
iyB
o�B
v�B
{�B
�=B
�bB
��B
��B
�fB
�B
=B#�B>wBR�Be`B��B�}B�B%B�B(�B7LB@�BJ�BW
B[#BbNBk�Bw�B�PB�B�qB�#B�B��B��B�/B��By�B6FB�BuBoBDB{B(�B1'B&�B�BVBDB��B�B�B��B��B�B��B�DB{�BO�B@�B�B
�ZB
��B
bNB
#�B
B	��B	�B	�B	�sB	�NB	�#B	��B	ǮB	�FB	��B	}�B	m�B	`BB	P�B	8RB	0!B	5?B	;dB	.B	�B	PB�B�B�sB�TB�;B�B��BɺBƨBÖB��B�qB�qB�qB�dB�RB�^B�XB�XB�RB�RB�XB�RB	+B��B	B	%B		7B	B��B��B��B�B�`B�HB�;B�B	VB		7B	JB	�B	"�B	�B	bB	
=B	B��B��B�B�TB�B��B��B��B�
B�5B�B�B��B��B	B	%B	
=B	hB	uB	�B	�B	�B	�B	�B	�B	&�B	&�B	&�B	%�B	$�B	"�B	!�B	%�B	$�B	�B	%�B	(�B	-B	-B	0!B	6FB	<jB	D�B	J�B	W
B	YB	XB	XB	ZB	[#B	_;B	e`B	gmB	gmB	hsB	iyB	m�B	o�B	r�B	v�B	w�B	x�B	y�B	z�B	}�B	~�B	}�B	y�B	t�B	� B	�B	�B	�B	~�B	|�B	z�B	x�B	v�B	w�B	{�B	u�B	s�B	v�B	{�B	�B	~�B	|�B	|�B	|�B	|�B	|�B	{�B	~�B	�B	�%B	�%B	�1B	�DB	�DB	�\B	�uB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�!B	�B	�B	�B	�B	�B	�3B	�-B	�!B	�'B	�?B	�'B	��B	��B	�B	�B	��B	��B	�B	�B	�B	�B	�B	�-B	�FB	�LB	�^B	�dB	�jB	�}B	��B	ÖB	ÖB	ĜB	ƨB	ǮB	ǮB	ɺB	ɺB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�#B	�#B	�#B	�#B	�#B	�#B	�B	�
B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�)B	�5B	�;B	�TB	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
  B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
%B
%B
%B
%B
+B
+B
+B
1B
1B
1B
	7B
DB
JB
JB
VB
\B
oB
oB
oB
oB
uB
uB
oB
bB
bB
hB
hB
uB
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
%FB
/�22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222B
H�B
H�B
H�B
I�B
H�B
H�B
H�B
H�B
C�B
@�B
:^B
9XB
9XB
9XB
:^B
:^B
:^B
9XB
8RB
?}B
E�B
[#B
q�B
`BB
ffB
gmB
k�B
m�B
k�B
jB
k�B
iyB
o�B
v�B
{�B
�=B
�bB
��B
��B
�fB
�B
=B#�B>wBR�Be`B��B�}B�B%B�B(�B7LB@�BJ�BW
B[#BbNBk�Bw�B�PB�B�qB�#B�B��B��B�/B��By�B6FB�BuBoBDB{B(�B1'B&�B�BVBDB��B�B�B��B��B�B��B�DB{�BO�B@�B�B
�ZB
��B
bNB
#�B
B	��B	�B	�B	�sB	�NB	�#B	��B	ǮB	�FB	��B	}�B	m�B	`BB	P�B	8RB	0!B	5?B	;dB	.B	�B	PB�B�B�sB�TB�;B�B��BɺBƨBÖB��B�qB�qB�qB�dB�RB�^B�XB�XB�RB�RB�XB�RB	+B��B	B	%B		7B	B��B��B��B�B�`B�HB�;B�B	VB		7B	JB	�B	"�B	�B	bB	
=B	B��B��B�B�TB�B��B��B��B�
B�5B�B�B��B��B	B	%B	
=B	hB	uB	�B	�B	�B	�B	�B	�B	&�B	&�B	&�B	%�B	$�B	"�B	!�B	%�B	$�B	�B	%�B	(�B	-B	-B	0!B	6FB	<jB	D�B	J�B	W
B	YB	XB	XB	ZB	[#B	_;B	e`B	gmB	gmB	hsB	iyB	m�B	o�B	r�B	v�B	w�B	x�B	y�B	z�B	}�B	~�B	}�B	y�B	t�B	� B	�B	�B	�B	~�B	|�B	z�B	x�B	v�B	w�B	{�B	u�B	s�B	v�B	{�B	�B	~�B	|�B	|�B	|�B	|�B	|�B	{�B	~�B	�B	�%B	�%B	�1B	�DB	�DB	�\B	�uB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�!B	�B	�B	�B	�B	�B	�3B	�-B	�!B	�'B	�?B	�'B	��B	��B	�B	�B	��B	��B	�B	�B	�B	�B	�B	�-B	�FB	�LB	�^B	�dB	�jB	�}B	��B	ÖB	ÖB	ĜB	ƨB	ǮB	ǮB	ɺB	ɺB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�#B	�#B	�#B	�#B	�#B	�#B	�B	�
B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�)B	�5B	�;B	�TB	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
  B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
%B
%B
%B
%B
+B
+B
+B
1B
1B
1B
	7B
DB
JB
JB
VB
\B
oB
oB
oB
oB
uB
uB
oB
bB
bB
hB
hB
uB
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
%FB
/�22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.33 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005190518                              AO  ARCAADJP                                                                    20181005190518    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005190518  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005190518  QCF$                G�O�G�O�G�O�8000            