CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:06:13Z creation      
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
_FillValue                 �  A<   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C0   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  T�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ^|   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  fH   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  h<   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  p   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  w�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  y�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �T   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �    HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �$   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �(   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �,   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �0Argo profile    3.1 1.2 19500101000000  20181005190613  20181005190613  5904952 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL              )A   AO  6431                            2B  A   APEX                            7466                            062512                          846 @��J	x1   @����� @0�t�j~��c~V�u1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                     )A   A   A   @9��@y��@�  A   A   A@  A`  A�  A�  A�  A�  A���A���A�  A�  B   B  BffBffB   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�33B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  C   C�fC�fC  C�C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C/�fC2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl�Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C��3C�  C�  C�  C��C�  C�  C�  C�  C��C�  C�  C�  C��C�  C��C�  C�  C�  C�  C��3C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C��3C��3C��3C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  D   D y�D ��D� DfD� D  D� D  D� D  D� D  Dy�D  D� D  D� D	  D	� D
  D
� D
��Dy�D��D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  Dy�D��D� D  D� D  Dy�D��D� D  D� D  Dy�D  D� DfD� D  Dy�D  D�fD fD � D!  D!� D!��D"� D#  D#� D$  D$� D$��D%� D&  D&�fD'  D'� D(  D(� D)  D)� D*  D*�fD+  D+� D,  D,� D-fD-� D.fD.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7�fD8  D8y�D9  D9� D:  D:y�D:��D;� D<  D<�fD=fD=�fD>  D>� D>��D?y�D@  D@� DA  DA� DB  DB� DC  DC� DD  DD�fDE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJfDJ� DK  DK� DL  DL� DM  DMy�DN  DN�fDOfDO�fDP  DP� DQ  DQ� DR  DR� DS  DSy�DS��DT� DT��DU� DV  DV� DW  DW� DX  DX�fDYfDY� DY��DZy�D[  D[� D\fD\� D]  D]� D^  D^� D_  D_y�D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Df��Dg� DhfDh�fDi  Di�fDjfDj� Dk  Dk� Dl  Dl� Dm  Dm�fDn  Dn� DofDo� Dp  Dp� Dq  Dq� DrfDr�fDsfDs� Dt  Dt� Dt��Duy�Dv  Dv� Dv��Dw� Dw��Dy��D�T)D�m1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @N�R@�\)@ʏ\AG�A%G�AEG�AeG�A���A���A���A���A�p�A�p�A��A��BQ�B	Q�B�RB�RB!Q�B)Q�B1Q�B9Q�BAQ�BIQ�BQQ�BYQ�BaQ�BiQ�BqQ�ByQ�B���B���B���B���B���B���B���B��)B���B���B���B���B���B��)B���B�u�B���BĨ�BȨ�B̨�BШ�BԨ�Bب�Bܨ�B��)B��B��B��B��B���B���B���C T{C:�C:�CT{CnC
T{CT{CT{CT{CT{CT{CT{CT{CT{CT{CT{C T{C"T{C$T{C&T{C(T{C*T{C,T{C.T{C0:�C2T{C4T{C6T{C8T{C:T{C<T{C>T{C@T{CBT{CDT{CFT{CHT{CJT{CLT{CNT{CPT{CRT{CTT{CVT{CXT{CZT{C\T{C^T{C`T{CbT{CdT{CfT{ChT{CjT{ClnCnT{CpT{CrT{CtT{CvT{CxT{CzT{C|T{C~T{C�*=C�pC�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�7
C�7
C�*=C�*=C�*=C�*=C�7
C�7
C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�7
C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�7
C�*=C�*=C�7
C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�7
C�*=C�pC�*=C�*=C�*=C�7
C�*=C�*=C�*=C�*=C�7
C�*=C�*=C�*=C�7
C�*=C�7
C�*=C�*=C�*=C�*=C�pC�*=C�*=C�7
C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�pC�*=C�*=C�pC�pC�pC�*=C�*=C�*=C�*=C�*=C�7
C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�7
C�*=C�*=D D ��D�D�D�D�DD�DD�DD�DD��DD�DD�D	D	�D
D
�D�D��D�D�DD�DD�DD�DD�DD�DD�DD��DD�DD��D�D�DD�DD��D�D�DD�DD��DD�D�D�DD��DD��D �D �D!D!�D"�D"�D#D#�D$D$�D%�D%�D&D&��D'D'�D(D(�D)D)�D*D*��D+D+�D,D,�D-�D-�D.�D.�D/D/�D0D0�D1D1�D2D2�D3D3�D4D4�D5D5�D6D6�D7D7��D8D8��D9D9�D:D:��D;�D;�D<D<��D=�D=��D>D>�D?�D?��D@D@�DADA�DBDB�DCDC�DDDD��DEDE�DFDF�DGDG�DHDH�DIDI�DJ�DJ�DKDK�DLDL�DMDM��DNDN��DO�DO��DPDP�DQDQ�DRDR�DSDS��DT�DT�DU�DU�DVDV�DWDW�DXDX��DY�DY�DZ�DZ��D[D[�D\�D\�D]D]�D^D^�D_D_��D`D`�DaDa�DbDb�DcDc�DdDd�DeDe�DfDf�Dg�Dg�Dh�Dh��DiDi��Dj�Dj�DkDk�DlDl�DmDm��DnDn�Do�Do�DpDp�DqDq�Dr�Dr��Ds�Ds�DtDt�Du�Du��DvDv�Dw�Dw�Dx�Dy��D�^�D�w�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�7LA�5?A�/A�7LA�$�A��A��A�+A���A��`A̰!A̡�A̲-A̧�A̡�A̲-A̶FA̶FA̩�A̮A̬A�jA˕�A�S�A�ƨAɗ�Aɰ!Aɴ9A�\)Aȗ�A�\)A�bA�K�A���A�~�A���AœuA�bNA�-A��`AĶFA�x�A�
=A�/A�bA��`A�1'A��A���A�r�A�ȴA�\)A�-A��A���A��jA���A�5?A��
A��A�K�A��jA���A�A�A�Q�A�l�A�hsA��A�
=A�bNA�\)A�K�A��A�(�A���A�5?A�(�A���A�ȴA�K�A��PA��+A�5?A�ĜA�ffA��A��A��;A��`A�A�A�?}A�x�A���A���A��`A�K�A��HA��\A��wA�I�A�ƨA�&�A���A�G�A��A���A��A�x�A}&�Av��Ar�An�!Al-AiG�Ae/A`�yA]VAXJAU�^AT�+ARE�AP��AOx�AN�+AI�hAF��AD�HAC33A@�A?�A;�^A:(�A9�A8��A8JA7O�A6�A4�uA2��A1/A01A.��A.JA-
=A,�A*ȴA(�9A($�A'��A'
=A&�`A&1'A%G�A$��A$^5A#7LA"��A!�A ��A��Av�A�HA+A�AZA��A��A�hA^5Ax�A�jA;dAv�A�AO�AoA�/A��A�A��A%A
��A
~�A
r�A
{A�uA�^AO�A�
Ax�A��AM�A33A"�AZA��AoA��A�`A��Ar�A1A�A ff@���@��@�
=@��`@�~�@��@���@�;d@�`B@� �@�S�@���@� �@��;@�?}@��@�X@�(�@���@�P@�/@�K�@���@���@ۥ�@��@���@�j@�(�@�t�@�o@�33@��@���@ָR@��@�5?@�Ĝ@Դ9@�I�@�o@�=q@Ѳ-@љ�@�x�@��#@�M�@�v�@�E�@ҏ\@ҸR@ҧ�@҇+@Ѻ^@�C�@ָR@ى7@��/@�5?@�V@�o@۾w@��
@ڰ!@�^5@�V@�M�@�E�@��@���@��#@١�@ش9@�z�@��@���@ו�@�K�@�o@��@և+@��@ՙ�@�X@��/@��@��y@�=q@Ѳ-@�X@��@��`@У�@�j@�b@���@�l�@���@�M�@���@�/@��`@̛�@�ƨ@�"�@��@ʸR@�~�@�5?@�{@�@��T@ɑh@�&�@ȼj@ȋD@�9X@���@ǶF@�\)@Ə\@�E�@�V@��T@��`@��;@��H@°!@§�@\@+@�n�@�^5@�M�@�$�@���@�x�@� �@�;d@���@��@�`B@��T@��^@���@�  @��P@�;d@��P@���@��@���@�r�@�1'@��w@�\)@�
=@���@�^5@�@��@�&�@�V@���@��`@���@�j@�9X@��
@�C�@��@��@�$�@�`B@���@��u@�S�@��@�o@�"�@���@�&�@���@�9X@��@�&�@��`@���@�A�@��@�C�@�
=@��H@���@��+@�M�@�$�@���@�X@��9@��@���@�;d@��H@���@��@���@�O�@�Ĝ@�I�@�1'@�b@��@��@�ƨ@��
@�\)@��R@��\@�v�@�-@�J@��7@�9X@���@���@���@�l�@��@���@�ff@��T@���@�7L@���@���@��j@���@�(�@��F@��@��@���@��P@�  @���@��@�"�@�S�@�t�@��w@�o@�-@��^@���@�@�-@�J@��#@���@���@���@�p�@�X@���@���@�Z@�1@�b@�  @���@�33@��@�ȴ@���@��y@���@���@�@��@��D@�I�@��@��
@��F@��P@�\)@�+@�@���@��R@���@���@��t@s�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�7LA�5?A�/A�7LA�$�A��A��A�+A���A��`A̰!A̡�A̲-A̧�A̡�A̲-A̶FA̶FA̩�A̮A̬A�jA˕�A�S�A�ƨAɗ�Aɰ!Aɴ9A�\)Aȗ�A�\)A�bA�K�A���A�~�A���AœuA�bNA�-A��`AĶFA�x�A�
=A�/A�bA��`A�1'A��A���A�r�A�ȴA�\)A�-A��A���A��jA���A�5?A��
A��A�K�A��jA���A�A�A�Q�A�l�A�hsA��A�
=A�bNA�\)A�K�A��A�(�A���A�5?A�(�A���A�ȴA�K�A��PA��+A�5?A�ĜA�ffA��A��A��;A��`A�A�A�?}A�x�A���A���A��`A�K�A��HA��\A��wA�I�A�ƨA�&�A���A�G�A��A���A��A�x�A}&�Av��Ar�An�!Al-AiG�Ae/A`�yA]VAXJAU�^AT�+ARE�AP��AOx�AN�+AI�hAF��AD�HAC33A@�A?�A;�^A:(�A9�A8��A8JA7O�A6�A4�uA2��A1/A01A.��A.JA-
=A,�A*ȴA(�9A($�A'��A'
=A&�`A&1'A%G�A$��A$^5A#7LA"��A!�A ��A��Av�A�HA+A�AZA��A��A�hA^5Ax�A�jA;dAv�A�AO�AoA�/A��A�A��A%A
��A
~�A
r�A
{A�uA�^AO�A�
Ax�A��AM�A33A"�AZA��AoA��A�`A��Ar�A1A�A ff@���@��@�
=@��`@�~�@��@���@�;d@�`B@� �@�S�@���@� �@��;@�?}@��@�X@�(�@���@�P@�/@�K�@���@���@ۥ�@��@���@�j@�(�@�t�@�o@�33@��@���@ָR@��@�5?@�Ĝ@Դ9@�I�@�o@�=q@Ѳ-@љ�@�x�@��#@�M�@�v�@�E�@ҏ\@ҸR@ҧ�@҇+@Ѻ^@�C�@ָR@ى7@��/@�5?@�V@�o@۾w@��
@ڰ!@�^5@�V@�M�@�E�@��@���@��#@١�@ش9@�z�@��@���@ו�@�K�@�o@��@և+@��@ՙ�@�X@��/@��@��y@�=q@Ѳ-@�X@��@��`@У�@�j@�b@���@�l�@���@�M�@���@�/@��`@̛�@�ƨ@�"�@��@ʸR@�~�@�5?@�{@�@��T@ɑh@�&�@ȼj@ȋD@�9X@���@ǶF@�\)@Ə\@�E�@�V@��T@��`@��;@��H@°!@§�@\@+@�n�@�^5@�M�@�$�@���@�x�@� �@�;d@���@��@�`B@��T@��^@���@�  @��P@�;d@��P@���@��@���@�r�@�1'@��w@�\)@�
=@���@�^5@�@��@�&�@�V@���@��`@���@�j@�9X@��
@�C�@��@��@�$�@�`B@���@��u@�S�@��@�o@�"�@���@�&�@���@�9X@��@�&�@��`@���@�A�@��@�C�@�
=@��H@���@��+@�M�@�$�@���@�X@��9@��@���@�;d@��H@���@��@���@�O�@�Ĝ@�I�@�1'@�b@��@��@�ƨ@��
@�\)@��R@��\@�v�@�-@�J@��7@�9X@���@���@���@�l�@��@���@�ff@��T@���@�7L@���@���@��j@���@�(�@��F@��@��@���@��P@�  @���@��@�"�@�S�@�t�@��w@�o@�-@��^@���@�@�-@�J@��#@���@���@���@�p�@�X@���@���@�Z@�1@�b@�  @���@�33@��@�ȴ@���@��y@���@���@�@��@��D@�I�@��@��
@��F@��P@�\)@�+@�@���@��R@���@���@��t@s�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
B
B
B
B
B	��B	��B
B	��B	��B	�B	��B	��B	��B	��B
B
B
B
B
B
+B
uB
!�B
F�B
W
B
�B
��B
�BB{B�BuBhB�B1'B?}BB�BB�BC�BF�BM�BP�BS�BZBl�Bt�B~�B� B�B�B�%B�B�+B�DB��B��B�'B�qB�}B��B�`B�B��BB{B(�B2-B.B0!B33B6FB;dB@�B@�B>wB2-B,B(�B&�B�B�B#�BuBB�`B��BǮB�FB��B��B��B��B� Bn�BC�B#�B	7B
�mB
�#B
�RB
��B
dZB
/B
B	�B	��B	�TB	�ZB	��B	��B	�JB	t�B	dZB	O�B	5?B	�B		7B�B�TB�#B��BȴBÖB�^B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�'B�-B�3B�?B�?B�?B�FB��BƨBŢBŢB��BȴBȴB��B��B��B��B��BƨB��B�wB�qB�}B��BŢB��B��B��B��B��B�
B�
B�
B�B�#B�)B�NB�mB�B�B�B�sB�ZB�BB�;B�5B�#B�B	B		7B		7B	+B	B	%B	
=B	JB	VB	VB	\B	JB	+B	B��B��B	�B	,B	+B	2-B	F�B	F�B	E�B	?}B	.B	 �B	!�B	�B	�B	�B	�B	\B	1B	+B	VB	hB	JB	
=B		7B	
=B	DB	
=B	JB	bB	oB	{B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	"�B	(�B	0!B	6FB	8RB	@�B	D�B	H�B	I�B	I�B	[#B	x�B	�VB	�JB	�B	��B	�B	�?B	�XB	�wB	�}B	��B	��B	��B	��B	��B	��B	��B	B	B	B	ÖB	ÖB	ÖB	ÖB	ÖB	ÖB	ŢB	ŢB	ŢB	ƨB	ƨB	ŢB	ÖB	ÖB	ĜB	ŢB	ĜB	ĜB	ŢB	ǮB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	�
B	�
B	�
B	�B	�B	�
B	�B	�B	�#B	�5B	�;B	�/B	�B	�B	�B	��B	�B	�)B	�B	�#B	�B	�#B	�BB	�;B	�5B	�5B	�;B	�;B	�BB	�BB	�NB	�TB	�TB	�TB	�TB	�ZB	�ZB	�`B	�`B	�`B	�`B	�mB	�yB	�B	�B	�B	�B	�B	�B	�yB	�yB	�sB	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B	��B	��B	��B	��B	��B
B
  B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
+B
B
%B
1B
1B
DB
VB
\B
bB
oB
oB
oB
uB
uB
uB
oB
oB
oB
uB
uB
oB
oB
hB
oB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
MB
5B
42222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 B
B
B
B
B
B	��B	��B
B	��B	��B	�B	��B	��B	��B	��B
B
B
B
B
B
+B
uB
!�B
F�B
W
B
�B
��B
�BB{B�BuBhB�B1'B?}BB�BB�BC�BF�BM�BP�BS�BZBl�Bt�B~�B� B�B�B�%B�B�+B�DB��B��B�'B�qB�}B��B�`B�B��BB{B(�B2-B.B0!B33B6FB;dB@�B@�B>wB2-B,B(�B&�B�B�B#�BuBB�`B��BǮB�FB��B��B��B��B� Bn�BC�B#�B	7B
�mB
�#B
�RB
��B
dZB
/B
B	�B	��B	�TB	�ZB	��B	��B	�JB	t�B	dZB	O�B	5?B	�B		7B�B�TB�#B��BȴBÖB�^B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�'B�-B�3B�?B�?B�?B�FB��BƨBŢBŢB��BȴBȴB��B��B��B��B��BƨB��B�wB�qB�}B��BŢB��B��B��B��B��B�
B�
B�
B�B�#B�)B�NB�mB�B�B�B�sB�ZB�BB�;B�5B�#B�B	B		7B		7B	+B	B	%B	
=B	JB	VB	VB	\B	JB	+B	B��B��B	�B	,B	+B	2-B	F�B	F�B	E�B	?}B	.B	 �B	!�B	�B	�B	�B	�B	\B	1B	+B	VB	hB	JB	
=B		7B	
=B	DB	
=B	JB	bB	oB	{B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	"�B	(�B	0!B	6FB	8RB	@�B	D�B	H�B	I�B	I�B	[#B	x�B	�VB	�JB	�B	��B	�B	�?B	�XB	�wB	�}B	��B	��B	��B	��B	��B	��B	��B	B	B	B	ÖB	ÖB	ÖB	ÖB	ÖB	ÖB	ŢB	ŢB	ŢB	ƨB	ƨB	ŢB	ÖB	ÖB	ĜB	ŢB	ĜB	ĜB	ŢB	ǮB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	�
B	�
B	�
B	�B	�B	�
B	�B	�B	�#B	�5B	�;B	�/B	�B	�B	�B	��B	�B	�)B	�B	�#B	�B	�#B	�BB	�;B	�5B	�5B	�;B	�;B	�BB	�BB	�NB	�TB	�TB	�TB	�TB	�ZB	�ZB	�`B	�`B	�`B	�`B	�mB	�yB	�B	�B	�B	�B	�B	�B	�yB	�yB	�sB	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B	��B	��B	��B	��B	��B
B
  B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
+B
B
%B
1B
1B
DB
VB
\B
bB
oB
oB
oB
uB
uB
uB
oB
oB
oB
uB
uB
oB
oB
hB
oB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
MB
5B
42222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.33 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005190613                              AO  ARCAADJP                                                                    20181005190613    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005190613  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005190613  QCF$                G�O�G�O�G�O�8000            