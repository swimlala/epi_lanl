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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20181005191755  20181005191755  5904954 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL              %A   AO  6557                            2B  A   APEX                            7468                            062512                          846 @����7�n1   @���>���@5P��
=q�d��Q�1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                     %A   A   A   @�ff@�  A   A   A@  A`  A�  A�  A�  A�33A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�33B�33B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C�C  C  C  C
  C  C  C  C  C  C  C  C  C�C  C   C"�C$�C&  C(  C*  C,  C.  C0  C2  C4  C6  C7�fC:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj�Cl  Cn  Cp  Cr�Ct�Cv  Cw�fCz  C{�fC~  C�  C�  C��C�  C�  C�  C��C��C�  C�  C�  C��3C�  C��C�  C��3C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C��C�  C��3C��3C��3C�  C��3C�  C�  C�  C�  C��3C�  C�  C�  C��C�  C�  C�  C��C��C��C�  C�  C�  C�  C�  C�  C��C�  C��3C�  C��C�  C�  C��C�  C�  C�  C�  C��C�  C��3C�  C�  C��3C�  C��C�  C��3C��3C��3C��3C�  C�  C��C�  C��C�  C��3C�  C�  C�  C�  C��C��C��C��C�  C�  C��C��3C�  C��3C��C��3C��C��3C��C�  C��3C��C��3D �fD ��D� D  D�fD  D� D  Dy�D  Dy�D  Dy�D  D� D��D� D	  D	�fD
fD
� D
�3D� D��Dy�DfD�fDfD�fD  D� D  D�fD  D� D  D� D  Dy�D  D� DfD�fD  D� D  D� D  D� DfD� D  D� D��D� D  D� D  D� D  D� D  D� D   D � D ��D!� D"  D"� D#  D#� D#��D$� D%  D%� D%��D&� D'fD'� D(  D(�fD)  D)y�D*  D*� D+  D+� D,  D,�fD-  D-� D.�D.� D/  D/y�D0  D0y�D1  D1y�D2  D2� D3  D3�fD4  D4�fD4��D5� D5��D6�fD7  D7�fD8  D8�fD9  D9� D:  D:� D;fD;� D<  D<� D=  D=y�D>  D>�fD>��D?� D?��D@� DA  DA� DBfDB� DC  DC�fDC��DDy�DEfDEy�DF  DF� DG  DGs3DG��DH� DH��DI�fDJ  DJ� DJ��DKy�DK��DL�fDM  DMy�DM��DN� DOfDO�fDO��DP� DQ  DQ�fDQ��DR� DS  DS� DTfDTs3DT��DU�fDV  DV� DW  DWy�DW�3DXy�DX��DY� DZ  DZ� D[fD[� D[��D\� D]  D]� D^  D^� D_  D_y�D_��D`� DafDay�Da��Db�fDc  Dcy�Dc��Ddy�De  De� Df  Df��DgfDgy�DhfDh�fDi�Di�fDi��Djy�Dj��Dk� DlfDl�fDm  Dm� Dn  Dn� Do  Do� DpfDp� Dp��Dqy�Dr  Dr�fDsfDsy�Ds��Dty�Du  Du� Dv  Dv�fDv��Dw� Dw�fDyx�D�>D�3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @���@ʏ\AG�A%G�AEG�AeG�A���A���A���A��
A£�Aң�A��A��BQ�B	Q�BQ�BQ�B!Q�B)Q�B1Q�B9Q�BAQ�BIQ�BQQ�BYQ�BaQ�BiQ�BqQ�ByQ�B���B���B���B���B���B�u�B���B���B���B���B��)B��)B���B��)B���B���B���BĨ�BȨ�B̨�BШ�BԨ�Bب�Bܨ�B��B��B��B��B��B���B���B���C T{CnCT{CT{CT{C
T{CT{CT{CT{CT{CT{CT{CT{CT{CnCT{C T{C"nC$nC&T{C(T{C*T{C,T{C.T{C0T{C2T{C4T{C6T{C8:�C:T{C<T{C>T{C@T{CBT{CDT{CFT{CHT{CJT{CLT{CNT{CPT{CRT{CTT{CVT{CXT{CZT{C\T{C^T{C`T{CbT{CdT{CfT{ChT{CjnClT{CnT{CpT{CrnCtnCvT{Cx:�CzT{C|:�C~T{C�*=C�*=C�7
C�*=C�*=C�*=C�7
C�7
C�*=C�*=C�*=C�pC�*=C�7
C�*=C�pC�*=C�*=C�*=C�*=C�*=C�*=C�7
C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�7
C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�7
C�7
C�*=C�*=C�7
C�*=C�pC�pC�pC�*=C�pC�*=C�*=C�*=C�*=C�pC�*=C�*=C�*=C�7
C�*=C�*=C�*=C�7
C�7
C�7
C�*=C�*=C�*=C�*=C�*=C�*=C�7
C�*=C�pC�*=C�7
C�*=C�*=C�7
C�*=C�*=C�*=C�*=C�7
C�*=C�pC�*=C�*=C�pC�*=C�7
C�*=C�pC�pC�pC�pC�*=C�*=C�7
C�*=C�7
C�*=C�pC�*=C�*=C�*=C�*=C�7
C�7
C�C�C�7
C�*=C�*=C�7
C�pC�*=C�pC�7
C�pC�7
C�pC�7
C�*=C�pC�7
D �D ��D�D�DD��DD�DD��DD��DD��DD�D�D�D	D	��D
�D
�DRD�D�D��D�D��D�D��DD�DD��DD�DD�DD��DD�D�D��DD�DD�DD�D�D�DD�D�D�DD�DD�DD�DD�D D �D!�D!�D"D"�D#D#�D$�D$�D%D%�D&�D&�D'�D'�D(D(��D)D)��D*D*�D+D+�D,D,��D-D-�D.!�D.�D/D/��D0D0��D1D1��D2D2�D3D3��D4D4��D5�D5�D6�D6��D7D7��D8D8��D9D9�D:D:�D;�D;�D<D<�D=D=��D>D>��D?�D?�D@�D@�DADA�DB�DB�DCDC��DD�DD��DE�DE��DFDF�DGDG�RDH�DH�DI�DI��DJDJ�DK�DK��DL�DL��DMDM��DN�DN�DO�DO��DP�DP�DQDQ��DR�DR�DSDS�DT�DT�RDU�DU��DVDV�DWDW��DXRDX��DY�DY�DZDZ�D[�D[�D\�D\�D]D]�D^D^�D_D_��D`�D`�Da�Da��Db�Db��DcDc��Dd�Dd��DeDe�DfDf��Dg�Dg��Dh�Dh��Di!�Di��Dj�Dj��Dk�Dk�Dl�Dl��DmDm�DnDn�DoDo�Dp�Dp�Dq�Dq��DrDr��Ds�Ds��Dt�Dt��DuDu�DvDv��Dw�Dw�DwۅDy�D�H�D�%�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��A��A���A�(�A� �A�dZA�$�A��FA���A�&�A��TA���A��A�l�A�Q�A�C�A�1'A� �A�1A��A��A��/A���A��+A�|�A�l�A�jA�jA�dZA�\)A�XA�1'A�%A��A���A�ffA���A�9XA��HA�ȴA���A�p�A�oA��jA��^A��uA�hsA��A�A��uA�r�A�M�A���A���A��mA�l�A�;dA��\A�hsA�A�l�A�bA�$�A���A�bNA�A�G�A�?}A���A��A���A���A��A�^5A�"�A���A�x�A�1A��^A��-A�jA���A�z�A��RA��jA�dZA�A�A��A��-A��A�G�A��A���A��+A�;dA��A���A�{A��A���A���A��TA�Q�A�  A��A��A�ZA���A�+A~9XA{�
A{`BAy�Awt�Au�#At��AtQ�AsG�Aq��Ap�Ao\)Ao�An��Am�Al��AlA�Ak+Aj$�Ah�HAh9XAg��Af=qAd�/Acp�Ab�!Aa�AaO�Aa�A`��A_A_hsA_"�A^JA]��A]x�A]l�A]XA]G�A\bAX�+AW33AV$�AUG�AR��AP�jANJALr�AK+AG�AE;dAC�mAA�A@�uA?��A?K�A>�RA>Q�A=�A<��A<�A;�7A:ZA8�A6�`A5C�A4M�A3A3�A3p�A2�A2E�A1��A0��A.��A-|�A,Q�A+��A+t�A*~�A(�RA&�jA%A"�yA!\)A bNA��A��AoAn�At�A�+A^5AO�AS�AQ�A�A�TAK�A��A/A�mA��A/A
��A	A�A1'A�jA+A��A
=AƨA ��A v�@��w@��-@��w@���@���@�bN@�+@�Ĝ@� �@���@�"�@�@���@��@��@�^@�-@���@�1'@�@�;d@⟾@�v�@�^5@�@�&�@�Ĝ@��@�V@���@� �@��H@ڇ+@�n�@ٺ^@��@أ�@�S�@��#@҇+@�hs@��@�
=@�`B@�t�@�ff@��#@ɉ7@�X@�Ĝ@�b@��
@�C�@�|�@ȼj@ɑh@ȋD@�Q�@���@�
=@��@���@�Ĝ@ă@Õ�@\@��@�ff@�7L@��@�@��@���@�l�@���@�{@�X@��`@���@�bN@�(�@���@��@��T@���@�O�@���@��;@�ƨ@�t�@�K�@�"�@�o@���@��\@�n�@�-@�@�v�@��\@�M�@�x�@�7L@��@��`@�Ĝ@��@�Z@�Q�@�1'@�|�@�ȴ@�~�@���@�ȴ@���@��@��^@�X@��@��9@�z�@�Z@�A�@� �@��m@���@�\)@�
=@��@��!@�{@��@�G�@��`@���@�I�@�(�@� �@��P@��y@�~�@�^5@�M�@�=q@�-@�@���@�x�@�p�@�X@��@���@��@�;d@��@��y@��R@�~�@�M�@��+@�~�@�E�@�5?@�$�@��@�J@���@��@�O�@�V@��/@��D@�j@�1'@���@���@��T@�p�@�&�@�Ĝ@�z�@�1'@�t�@�
=@�n�@�$�@�J@���@�X@�V@�%@��@���@��9@��@��D@�bN@�I�@�9X@� �@��@��m@�ƨ@���@�|�@�t�@�C�@�33@�+@�"�@�o@���@�n�@�V@�=q@�$�@��@�{@���@���@��h@�hs@�O�@��@���@�z�@�j@�b@��P@�t�@�dZ@�"�@���@��H@�ȴ@��!@��+@�-@�@��7@�hs@�7L@��@���@���@�bN@�(�@� �@�(�@�1'@�  @�S�@�o@�;d@�@��!@�~�@�M�@�J@���@�X@�`B@�/@��@�j@�A�@��w@�o@�o@��H@�N�@z	@g��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��A��A���A�(�A� �A�dZA�$�A��FA���A�&�A��TA���A��A�l�A�Q�A�C�A�1'A� �A�1A��A��A��/A���A��+A�|�A�l�A�jA�jA�dZA�\)A�XA�1'A�%A��A���A�ffA���A�9XA��HA�ȴA���A�p�A�oA��jA��^A��uA�hsA��A�A��uA�r�A�M�A���A���A��mA�l�A�;dA��\A�hsA�A�l�A�bA�$�A���A�bNA�A�G�A�?}A���A��A���A���A��A�^5A�"�A���A�x�A�1A��^A��-A�jA���A�z�A��RA��jA�dZA�A�A��A��-A��A�G�A��A���A��+A�;dA��A���A�{A��A���A���A��TA�Q�A�  A��A��A�ZA���A�+A~9XA{�
A{`BAy�Awt�Au�#At��AtQ�AsG�Aq��Ap�Ao\)Ao�An��Am�Al��AlA�Ak+Aj$�Ah�HAh9XAg��Af=qAd�/Acp�Ab�!Aa�AaO�Aa�A`��A_A_hsA_"�A^JA]��A]x�A]l�A]XA]G�A\bAX�+AW33AV$�AUG�AR��AP�jANJALr�AK+AG�AE;dAC�mAA�A@�uA?��A?K�A>�RA>Q�A=�A<��A<�A;�7A:ZA8�A6�`A5C�A4M�A3A3�A3p�A2�A2E�A1��A0��A.��A-|�A,Q�A+��A+t�A*~�A(�RA&�jA%A"�yA!\)A bNA��A��AoAn�At�A�+A^5AO�AS�AQ�A�A�TAK�A��A/A�mA��A/A
��A	A�A1'A�jA+A��A
=AƨA ��A v�@��w@��-@��w@���@���@�bN@�+@�Ĝ@� �@���@�"�@�@���@��@��@�^@�-@���@�1'@�@�;d@⟾@�v�@�^5@�@�&�@�Ĝ@��@�V@���@� �@��H@ڇ+@�n�@ٺ^@��@أ�@�S�@��#@҇+@�hs@��@�
=@�`B@�t�@�ff@��#@ɉ7@�X@�Ĝ@�b@��
@�C�@�|�@ȼj@ɑh@ȋD@�Q�@���@�
=@��@���@�Ĝ@ă@Õ�@\@��@�ff@�7L@��@�@��@���@�l�@���@�{@�X@��`@���@�bN@�(�@���@��@��T@���@�O�@���@��;@�ƨ@�t�@�K�@�"�@�o@���@��\@�n�@�-@�@�v�@��\@�M�@�x�@�7L@��@��`@�Ĝ@��@�Z@�Q�@�1'@�|�@�ȴ@�~�@���@�ȴ@���@��@��^@�X@��@��9@�z�@�Z@�A�@� �@��m@���@�\)@�
=@��@��!@�{@��@�G�@��`@���@�I�@�(�@� �@��P@��y@�~�@�^5@�M�@�=q@�-@�@���@�x�@�p�@�X@��@���@��@�;d@��@��y@��R@�~�@�M�@��+@�~�@�E�@�5?@�$�@��@�J@���@��@�O�@�V@��/@��D@�j@�1'@���@���@��T@�p�@�&�@�Ĝ@�z�@�1'@�t�@�
=@�n�@�$�@�J@���@�X@�V@�%@��@���@��9@��@��D@�bN@�I�@�9X@� �@��@��m@�ƨ@���@�|�@�t�@�C�@�33@�+@�"�@�o@���@�n�@�V@�=q@�$�@��@�{@���@���@��h@�hs@�O�@��@���@�z�@�j@�b@��P@�t�@�dZ@�"�@���@��H@�ȴ@��!@��+@�-@�@��7@�hs@�7L@��@���@���@�bN@�(�@� �@�(�@�1'@�  @�S�@�o@�;d@�@��!@�~�@�M�@�J@���@�X@�`B@�/@��@�j@�A�@��w@�o@�o@��H@�N�@z	@g��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B|�B|�B{�Bu�Br�Bp�Bp�Bt�B�+B��B��B��B��B��B��B��B��B��B��B��B�B�B��B��B��B��B��B��B��B��B�B�B��B�-B�'B�'B�!B�!B�LB�}BɺB��B�
B�BB�HB�`B�mB�B�B�B�yB�sB�B��B+BuB�B�B�B!�B�B�B	7B��B��B��B�B��BȴBǮBB�}B�jB�XB�FB�?B�RB�-B�wB��B�dB��B�BaHBP�B9XB�B%B�TBȴB�XB��B�+Bx�Bq�B[#BL�B8RB#�B�B	7B
��B
��B
�B
�NB
�qB
��B
��B
�bB
�B
s�B
o�B
dZB
VB
J�B
D�B
?}B
9XB
.B
'�B
�B
�B
�B
�B
PB
	7B
B	��B	�B	�B	�yB	�;B	�B	��B	ɺB	ŢB	��B	��B	�qB	�LB	�?B	�-B	�B	��B	��B	��B	��B	��B	��B	�B	y�B	s�B	m�B	`BB	R�B	?}B	2-B	$�B	PB	B��B��B�B�B�sB�TB�BB�5B�BB�BB�5B�B��BǮB�}B�jB�XB�XB�LB�?B�-B�B��B��B��B��B��B�{B�bB�=B�B|�Bv�Br�Bo�Bl�Bm�Bm�Bn�Bq�Bs�Bs�Bp�Bo�Bl�Bp�Bp�Bo�Bn�Bm�Bk�BjBhsBffBdZBaHB_;BZBYBYBZBZBYBYBXB[#BgmBo�Br�Br�Bt�Bv�Bv�Bv�Bv�Bv�Bv�Bv�Bt�Br�Bp�Bo�Bm�Bm�Bo�Bp�Bp�Bp�Bo�Bp�Br�Bq�Bp�Bo�Bn�Bp�Bp�Bp�Bq�Bq�Bp�Bo�Bk�BjBiyBjBiyBk�Bn�Bq�Bs�Bt�Bt�Bv�B{�B~�B�B�\B��B��B��B��B��B�B�B�B�'B�'B�3B�?B�RB�dB�wBÖB��B��B��B�B�B�B�)B�BB�HB�NB�TB�ZB�NB�HB�NB�fB�sB�B�B�B�B�B��B��B��B��B��B	B	DB	VB	bB	oB	{B	�B	�B	�B	�B	�B	�B	�B	$�B	&�B	(�B	.B	2-B	5?B	7LB	7LB	8RB	8RB	:^B	<jB	=qB	>wB	@�B	A�B	E�B	G�B	J�B	K�B	L�B	Q�B	T�B	VB	ZB	^5B	bNB	dZB	ffB	iyB	l�B	o�B	p�B	q�B	q�B	r�B	s�B	w�B	x�B	x�B	z�B	~�B	� B	�B	�B	�B	�7B	�DB	�PB	�hB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	�B	�B	�'B	�-B	�FB	�RB	�XB	�jB	�wB	�}B	��B	��B	B	ÖB	ÖB	ÖB	ĜB	ŢB	ƨB	ȴB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�
B	�
B	�B	�B	�#B	�#B	�/B	�5B	�5B	�5B	�;B	�NB	�NB	�NB	�ZB	�`B	�`B	�fB	�fB	�mB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
+B
+B
1B
1B
PB

�B
!B
/5222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  B|�B|�B{�Bu�Br�Bp�Bp�Bt�B�+B��B��B��B��B��B��B��B��B��B��B��B�B�B��B��B��B��B��B��B��B��B�B�B��B�-B�'B�'B�!B�!B�LB�}BɺB��B�
B�BB�HB�`B�mB�B�B�B�yB�sB�B��B+BuB�B�B�B!�B�B�B	7B��B��B��B�B��BȴBǮBB�}B�jB�XB�FB�?B�RB�-B�wB��B�dB��B�BaHBP�B9XB�B%B�TBȴB�XB��B�+Bx�Bq�B[#BL�B8RB#�B�B	7B
��B
��B
�B
�NB
�qB
��B
��B
�bB
�B
s�B
o�B
dZB
VB
J�B
D�B
?}B
9XB
.B
'�B
�B
�B
�B
�B
PB
	7B
B	��B	�B	�B	�yB	�;B	�B	��B	ɺB	ŢB	��B	��B	�qB	�LB	�?B	�-B	�B	��B	��B	��B	��B	��B	��B	�B	y�B	s�B	m�B	`BB	R�B	?}B	2-B	$�B	PB	B��B��B�B�B�sB�TB�BB�5B�BB�BB�5B�B��BǮB�}B�jB�XB�XB�LB�?B�-B�B��B��B��B��B��B�{B�bB�=B�B|�Bv�Br�Bo�Bl�Bm�Bm�Bn�Bq�Bs�Bs�Bp�Bo�Bl�Bp�Bp�Bo�Bn�Bm�Bk�BjBhsBffBdZBaHB_;BZBYBYBZBZBYBYBXB[#BgmBo�Br�Br�Bt�Bv�Bv�Bv�Bv�Bv�Bv�Bv�Bt�Br�Bp�Bo�Bm�Bm�Bo�Bp�Bp�Bp�Bo�Bp�Br�Bq�Bp�Bo�Bn�Bp�Bp�Bp�Bq�Bq�Bp�Bo�Bk�BjBiyBjBiyBk�Bn�Bq�Bs�Bt�Bt�Bv�B{�B~�B�B�\B��B��B��B��B��B�B�B�B�'B�'B�3B�?B�RB�dB�wBÖB��B��B��B�B�B�B�)B�BB�HB�NB�TB�ZB�NB�HB�NB�fB�sB�B�B�B�B�B��B��B��B��B��B	B	DB	VB	bB	oB	{B	�B	�B	�B	�B	�B	�B	�B	$�B	&�B	(�B	.B	2-B	5?B	7LB	7LB	8RB	8RB	:^B	<jB	=qB	>wB	@�B	A�B	E�B	G�B	J�B	K�B	L�B	Q�B	T�B	VB	ZB	^5B	bNB	dZB	ffB	iyB	l�B	o�B	p�B	q�B	q�B	r�B	s�B	w�B	x�B	x�B	z�B	~�B	� B	�B	�B	�B	�7B	�DB	�PB	�hB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	�B	�B	�'B	�-B	�FB	�RB	�XB	�jB	�wB	�}B	��B	��B	B	ÖB	ÖB	ÖB	ĜB	ŢB	ƨB	ȴB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�
B	�
B	�B	�B	�#B	�#B	�/B	�5B	�5B	�5B	�;B	�NB	�NB	�NB	�ZB	�`B	�`B	�fB	�fB	�mB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
+B
+B
1B
1B
PB

�B
!B
/5222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.33 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005191755                              AO  ARCAADJP                                                                    20181005191755    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005191755  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005191755  QCF$                G�O�G�O�G�O�8000            