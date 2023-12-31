CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:17:08Z creation      
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
_FillValue                    �@Argo profile    3.1 1.2 19500101000000  20181005191708  20181005191708  5904954 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               WA   AO  6557                            2B  A   APEX                            7468                            062512                          846 @��$ƻl�1   @��%M��	@4���v��d5��l�D1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      WA   A   A   @,��@�  @�  A   A   A@  Aa��A�  A�  A�  A�  A�  A�  A�  A�  B   B  BffB  B��B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C�fC   C"�C$�C&  C(  C*�C,  C.  C0  C1�fC4  C6  C8  C9�fC<  C>  C@  CB  CD  CF  Cv�Cx  Cz  C|  C~  C�  C��3C��3C�  C�  C�  C��3C��3C��3C��3C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C��3C��C��C��3C��fC�  C��C��C��3C��3C��C�  C�  C�  C�  C�  C�  C��C��C��C��C��C�  C��C��C�  C��C�  C��C�  C��3C��3C��3C�  C�  C�  C�  C�  C�  C�  C��C��C��C�  C�  C�  C��3C��3C�  C�  C�  C�  C��3C��3C�  C��C��C��C�  C��fC��fC��3C�  C�  C��C��C�  C��3C��3C��3C��C�  C�  C�  C��C��C�  C�  C��3C��3C��C�  C��3C��3C��3C��C�  C�  C��C�  C�  C�  C�  C��C�  C��C��C�  C�  C�  C��C��C��D   D �fD  D� D  D� D  Dy�D  D� D��Dy�D  D�fDfD� D  D� D	  D	y�D	��D
� DfD� D  Dy�D��D� D  Dy�D  D�fDfD� D  Dy�DfD�fD  D�fD  D�fDfD� D  D� D  D�fDfD� D  D� D��D� D  D� D  Dy�D  D� D  D�fDfD� D   D y�D!  D!�fD"fD"� D"��D#� D$  D$� D$��D%y�D%��D&� D'  D'y�D(  D(� D)fD)�fD*  D*y�D*��D+� D,  D,� D-  D-y�D.  D.y�D.��D/y�D0  D0� D1fD1�fD2  D2� D3fD3�fD4  D4� D4��D5� D6  D6� D7  D7� D8�D8�fD8��D9� D:  D:� D;�D;�fD<fD<� D=  D=� D>  D>� D?  D?� D@fD@� DA  DA� DBfDB� DC  DC�fDD  DD�fDEfDE��DF  DFy�DG  DG�fDH  DH� DIfDI� DI�3DJ� DKfDK� DK��DLy�DL��DMy�DN  DNy�DO  DO� DP  DP�fDQ  DQ� DR  DR�fDSfDS� DS��DTy�DU  DU�fDV�DV�fDW  DW� DX  DX� DYfDY�fDZ  DZy�D[  D[�fD\  D\�fD]fD]�fD^fD^� D_fD_�fD`  D`�fDafDa��DbfDb� DcfDcy�Dc��Dd� De  De�fDe��Dfy�Dg  Dgy�Dh  Dh�fDifDi�fDj  Dj� Dj��Dk� Dl  Dl�fDmfDm� Dn  Dn� Dn�3Doy�Dp  Dp� Dp��Dq� DrfDr�fDsfDs� DtfDty�Du  Du�fDvfDv� Dw  Dw� Dy�=D�%qD��
1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @A�@��\@ʏ\AG�A%G�AEG�Af�HA���A���A���A���A£�Aң�A��A��BQ�B	Q�B�RBQ�B �B)Q�B1Q�B9Q�BAQ�BIQ�BQQ�BYQ�BaQ�BiQ�BqQ�ByQ�B���B���B���B���B���B���B���B���B���B���B���B���B���B�u�B���B���B���BĨ�BȨ�B̨�BШ�BԨ�Bب�Bܨ�B��B��B��B��B��B���B���B���C T{CT{CT{CT{CT{C
T{CT{CT{CT{CT{CT{CT{CT{CT{CT{C:�C T{C"nC$nC&T{C(T{C*nC,T{C.T{C0T{C2:�C4T{C6T{C8T{C::�C<T{C>T{C@T{CBT{CDT{CFT{CvnCxT{CzT{C|T{C~T{C�*=C�pC�pC�*=C�*=C�*=C�pC�pC�pC�pC�*=C�*=C�pC�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�pC�pC�*=C�*=C�pC�7
C�7
C�pC��C�*=C�7
C�7
C�pC�pC�7
C�*=C�*=C�*=C�*=C�*=C�*=C�7
C�7
C�C�C�C�C�7
C�*=C�7
C�7
C�*=C�7
C�*=C�7
C�*=C�pC�pC�pC�*=C�*=C�*=C�*=C�*=C�*=C�*=C�7
C�7
C�7
C�*=C�*=C�*=C�pC�pC�*=C�*=C�*=C�*=C�pC�pC�*=C�7
C�7
C�7
C�*=C��C��C�pC�*=C�*=C�7
C�7
C�*=C�pC�pC�pC�7
C�*=C�*=C�*=C�7
C�7
C�*=C�*=C�pC�pC�7
C�*=C�pC�pC�pC�7
C�*=C�*=C�7
C�*=C�*=C�*=C�*=C�7
C�*=C�7
C�7
C�*=C�*=C�*=C�7
C�7
C�7
D D ��DD�DD�DD��DD�D�D��DD��D�D�DD�D	D	��D
�D
�D�D�DD��D�D�DD��DD��D�D�DD��D�D��DD��DD��D�D�DD�DD��D�D�DD�D�D�DD�DD��DD�DD��D�D�D D ��D!D!��D"�D"�D#�D#�D$D$�D%�D%��D&�D&�D'D'��D(D(�D)�D)��D*D*��D+�D+�D,D,�D-D-��D.D.��D/�D/��D0D0�D1�D1��D2D2�D3�D3��D4D4�D5�D5�D6D6�D7D7�D8!�D8��D9�D9�D:D:�D;!�D;��D<�D<�D=D=�D>D>�D?D?�D@�D@�DADA�DB�DB�DCDC��DDDD��DE�DE��DFDF��DGDG��DHDH�DI�DI�DJRDJ�DK�DK�DL�DL��DM�DM��DNDN��DODO�DPDP��DQDQ�DRDR��DS�DS�DT�DT��DUDU��DV!�DV��DWDW�DXDX�DY�DY��DZDZ��D[D[��D\D\��D]�D]��D^�D^�D_�D_��D`D`��Da�Da��Db�Db�Dc�Dc��Dd�Dd�DeDe��Df�Df��DgDg��DhDh��Di�Di��DjDj�Dk�Dk�DlDl��Dm�Dm�DnDn�DoRDo��DpDp�Dq�Dq�Dr�Dr��Ds�Ds�Dt�Dt��DuDu��Dv�Dv�DwDw�Dy�\D�0 D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��HA��HA��HA��HA��HA��TA��`A��`A��`A��`A��HA���A��#A�Aܴ9Aܛ�A܅A�;dAډ7A�7LA�jA�A͛�A�E�Ḁ�A˼jA�
=A�&�Aƺ^A�ȴA��mA�%A��A�7LA�bA��FA�|�A�VA��-A�S�A�ȴA��A�JA�
=A�S�A��A��A�ƨA��A�A�~�A�VA��A�  A�hsA�1'A��/A��-A�  A�ZA�^5A���A�XA�t�A�&�A�S�A�/A�ĜA�ȴA�XA�ffA�z�A�A���A���A�A�A�x�A��\A���A�|�A��A�7LA�M�A��
A���A�bNA�t�A�{A�I�A�%A��+A��^A7LAx-AuXAr�DAq%AmS�Aj~�AA/A@��A?K�A=�TA<r�A<E�A;�A9l�A6�A4��A3G�A2�\A1�;A1��A1��A1l�A0VA/��A/��A/|�A/\)A.r�A,��A+33A)�#A(�yA(z�A'K�A&E�A%
=A#XA!�A ZA�#A�HA1'AJAG�A`BA��AC�A�wA��A�
A�AM�AC�A1'A��A�`Av�A�#A	�A��A1'A�yA�A��A/An�A(�AJA�;AC�A�hA �DA M�@���@�o@��\@�7L@�A�@��P@�@�ȴ@�p�@��;@�p�@��@�ff@�9@띲@��@���@�p�@蛦@���@�l�@�v�@噚@�&�@�u@�b@���@�7@��/@�(�@ߥ�@�ȴ@�M�@ݩ�@�&�@ܣ�@۶F@�n�@ٺ^@��@�z�@ו�@֟�@ա�@��`@��m@�C�@��@�^5@�/@���@�j@϶F@�dZ@�
=@Ώ\@�Ĝ@�v�@���@�G�@��@ǍP@�K�@ư!@�ff@�{@Ų-@�&�@��@Ĭ@�  @+@���@��j@��m@�t�@�~�@�$�@���@��@���@��^@�%@��D@��@�@�~�@�E�@�@�`B@��j@��@��@�$�@�@���@�p�@��@�hs@�r�@��@�;d@���@�^5@�{@��@�K�@�C�@� �@�?}@�A�@���@��T@��@�\)@�O�@�O�@�G�@���@�v�@�=q@���@���@��h@��/@�Z@�Z@�Z@��j@��9@��@��D@��j@��7@�`B@���@�`B@�hs@���@�dZ@�t�@���@�Q�@���@��j@�K�@���@��+@���@���@�$�@��R@���@��!@���@��\@�~�@�~�@�~�@�~�@�v�@�~�@�v�@�ff@�=q@�{@��@���@��h@�O�@�G�@�&�@��@��9@�I�@��w@�dZ@�;d@�"�@�"�@�"�@��H@���@�~�@�V@�$�@�{@���@�`B@��@�Ĝ@���@��@�z�@�bN@�A�@�b@��F@���@��@��@��F@��@�|�@�l�@�"�@��@���@���@�=q@��@��#@���@���@���@�p�@�`B@�O�@�7L@�/@���@��@�I�@�(�@��@��F@�S�@�"�@��@��@��!@�n�@�E�@�$�@�J@���@�&�@���@�Z@�9X@�(�@�ƨ@�dZ@�+@�"�@��@�ȴ@��\@�5?@���@���@��7@�&�@��/@��@��@��@���@���@�|�@�33@��R@�=q@���@�/@�%@��`@�Ĝ@�r�@�b@���@��@���@�;d@���@�E�@�@�@���@�X@�?}@�&�@�%@��@��`@���@���@�Ĝ@��@��@�1'@���@��;@��F@��P@�t�@�K�@�;d@�+@�"�@��@�o@�@��y@��R@���@�7�@v �@b��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A��HA��HA��HA��HA��HA��TA��`A��`A��`A��`A��HA���A��#A�Aܴ9Aܛ�A܅A�;dAډ7A�7LA�jA�A͛�A�E�Ḁ�A˼jA�
=A�&�Aƺ^A�ȴA��mA�%A��A�7LA�bA��FA�|�A�VA��-A�S�A�ȴA��A�JA�
=A�S�A��A��A�ƨA��A�A�~�A�VA��A�  A�hsA�1'A��/A��-A�  A�ZA�^5A���A�XA�t�A�&�A�S�A�/A�ĜA�ȴA�XA�ffA�z�A�A���A���A�A�A�x�A��\A���A�|�A��A�7LA�M�A��
A���A�bNA�t�A�{A�I�A�%A��+A��^A7LAx-AuXAr�DAq%AmS�Aj~�AA/A@��A?K�A=�TA<r�A<E�A;�A9l�A6�A4��A3G�A2�\A1�;A1��A1��A1l�A0VA/��A/��A/|�A/\)A.r�A,��A+33A)�#A(�yA(z�A'K�A&E�A%
=A#XA!�A ZA�#A�HA1'AJAG�A`BA��AC�A�wA��A�
A�AM�AC�A1'A��A�`Av�A�#A	�A��A1'A�yA�A��A/An�A(�AJA�;AC�A�hA �DA M�@���@�o@��\@�7L@�A�@��P@�@�ȴ@�p�@��;@�p�@��@�ff@�9@띲@��@���@�p�@蛦@���@�l�@�v�@噚@�&�@�u@�b@���@�7@��/@�(�@ߥ�@�ȴ@�M�@ݩ�@�&�@ܣ�@۶F@�n�@ٺ^@��@�z�@ו�@֟�@ա�@��`@��m@�C�@��@�^5@�/@���@�j@϶F@�dZ@�
=@Ώ\@�Ĝ@�v�@���@�G�@��@ǍP@�K�@ư!@�ff@�{@Ų-@�&�@��@Ĭ@�  @+@���@��j@��m@�t�@�~�@�$�@���@��@���@��^@�%@��D@��@�@�~�@�E�@�@�`B@��j@��@��@�$�@�@���@�p�@��@�hs@�r�@��@�;d@���@�^5@�{@��@�K�@�C�@� �@�?}@�A�@���@��T@��@�\)@�O�@�O�@�G�@���@�v�@�=q@���@���@��h@��/@�Z@�Z@�Z@��j@��9@��@��D@��j@��7@�`B@���@�`B@�hs@���@�dZ@�t�@���@�Q�@���@��j@�K�@���@��+@���@���@�$�@��R@���@��!@���@��\@�~�@�~�@�~�@�~�@�v�@�~�@�v�@�ff@�=q@�{@��@���@��h@�O�@�G�@�&�@��@��9@�I�@��w@�dZ@�;d@�"�@�"�@�"�@��H@���@�~�@�V@�$�@�{@���@�`B@��@�Ĝ@���@��@�z�@�bN@�A�@�b@��F@���@��@��@��F@��@�|�@�l�@�"�@��@���@���@�=q@��@��#@���@���@���@�p�@�`B@�O�@�7L@�/@���@��@�I�@�(�@��@��F@�S�@�"�@��@��@��!@�n�@�E�@�$�@�J@���@�&�@���@�Z@�9X@�(�@�ƨ@�dZ@�+@�"�@��@�ȴ@��\@�5?@���@���@��7@�&�@��/@��@��@��@���@���@�|�@�33@��R@�=q@���@�/@�%@��`@�Ĝ@�r�@�b@���@��@���@�;d@���@�E�@�@�@���@�X@�?}@�&�@�%@��@��`@���@���@�Ĝ@��@��@�1'@���@��;@��F@��P@�t�@�K�@�;d@�+@�"�@��@�o@�@��y@��R@���@�7�@v �@b��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B)�B)�B)�B)�B+B+B+B+B)�B)�B)�B)�B)�B(�B'�B&�B%�B"�B�B	7B��B�B��B  BB+B�B(�B8RBA�BW
Bo�Bx�B~�B�+B�\B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�PB�Bz�Bu�Bv�BjBM�B2-B(�B$�B�BbB  B�B�/BÖB�?B��B�Be`BO�BB�B6FB,B"�B+B
�B
�ZB
�)B
�
B
��B
ƨB
��B
��B
��B
�JB
o�B
;dB
B	�B	�BB	��B	�qBs�B��B��B��BȴB��BɺBƨB�^B�!B�-B�!B�!B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B�{B�oB�hB�bB�bB�bB�\B�\B�hB�uB�oB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�uB�hB�\B�hB�PB�PB�DB�=B�7B�1B�1B�1B�1B�7B�DB�JB�VB�PB�PB�hB�{B�{B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�3B�9B�LB�XB�^B�dB�}B�}B��BBĜBŢBƨB��B��B��B��B�B�#B�)B�5B�5B�;B�;B�HB�HB�HB�NB�B�B�B�B��B��B��B��B��B��B��B	B	B	+B	
=B	\B	hB	uB	�B	�B	�B	�B	�B	!�B	&�B	'�B	(�B	(�B	+B	.B	/B	0!B	0!B	0!B	.B	!�B	"�B	(�B	33B	8RB	49B	49B	1'B	-B	(�B	5?B	;dB	5?B	9XB	:^B	>wB	A�B	B�B	B�B	D�B	J�B	P�B	T�B	YB	`BB	bNB	hsB	o�B	p�B	s�B	t�B	y�B	u�B	u�B	v�B	y�B	� B	�+B	�1B	�B	�B	�B	�B	�%B	�7B	�bB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�'B	�9B	�?B	�?B	�?B	�?B	�LB	�RB	�XB	�^B	�dB	�jB	�qB	�}B	��B	B	B	ÖB	ÖB	ÖB	ĜB	ƨB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�B	�B	�B	�B	�B	�B	�#B	�#B	�)B	�/B	�;B	�NB	�TB	�ZB	�ZB	�`B	�mB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
%B
%B
%B
+B
	7B

=B

=B

=B

=B
DB
JB
JB
PB
PB
VB
VB
VB
VB
\B
\B
\B
\B
\B
\B
\B
bB
bB
bB
hB
hB
hB
hB
oB
oB
oB
oB
oB
oB
oB
oB
uB
�B
dB
*02222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 B)�B)�B)�B)�B+B+B+B+B)�B)�B)�B)�B)�B(�B'�B&�B%�B"�B�B	7B��B�B��B  BB+B�B(�B8RBA�BW
Bo�Bx�B~�B�+B�\B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�PB�Bz�Bu�Bv�BjBM�B2-B(�B$�B�BbB  B�B�/BÖB�?B��B�Be`BO�BB�B6FB,B"�B+B
�B
�ZB
�)B
�
B
��B
ƨB
��B
��B
��B
�JB
o�B
;dB
B	�B	�BB	��B	�qBs�B��B��B��BȴB��BɺBƨB�^B�!B�-B�!B�!B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B�{B�oB�hB�bB�bB�bB�\B�\B�hB�uB�oB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�uB�hB�\B�hB�PB�PB�DB�=B�7B�1B�1B�1B�1B�7B�DB�JB�VB�PB�PB�hB�{B�{B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�3B�9B�LB�XB�^B�dB�}B�}B��BBĜBŢBƨB��B��B��B��B�B�#B�)B�5B�5B�;B�;B�HB�HB�HB�NB�B�B�B�B��B��B��B��B��B��B��B	B	B	+B	
=B	\B	hB	uB	�B	�B	�B	�B	�B	!�B	&�B	'�B	(�B	(�B	+B	.B	/B	0!B	0!B	0!B	.B	!�B	"�B	(�B	33B	8RB	49B	49B	1'B	-B	(�B	5?B	;dB	5?B	9XB	:^B	>wB	A�B	B�B	B�B	D�B	J�B	P�B	T�B	YB	`BB	bNB	hsB	o�B	p�B	s�B	t�B	y�B	u�B	u�B	v�B	y�B	� B	�+B	�1B	�B	�B	�B	�B	�%B	�7B	�bB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�'B	�9B	�?B	�?B	�?B	�?B	�LB	�RB	�XB	�^B	�dB	�jB	�qB	�}B	��B	B	B	ÖB	ÖB	ÖB	ĜB	ƨB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�B	�B	�B	�B	�B	�B	�#B	�#B	�)B	�/B	�;B	�NB	�TB	�ZB	�ZB	�`B	�mB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
%B
%B
%B
+B
	7B

=B

=B

=B

=B
DB
JB
JB
PB
PB
VB
VB
VB
VB
\B
\B
\B
\B
\B
\B
\B
bB
bB
bB
hB
hB
hB
hB
oB
oB
oB
oB
oB
oB
oB
oB
uB
�B
dB
*02222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.33 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005191708                              AO  ARCAADJP                                                                    20181005191708    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005191708  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005191708  QCF$                G�O�G�O�G�O�8000            