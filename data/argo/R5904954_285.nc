CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:17:54Z creation      
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
_FillValue                    �0Argo profile    3.1 1.2 19500101000000  20181005191754  20181005191754  5904954 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL              A   AO  6557                            2B  A   APEX                            7468                            062512                          846 @����b��1   @���ww�0@5R� ě��d}�E��1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                     A   A   A   @333@�  @�  @���A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   BffB  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C!�fC$  C&  C'�fC*  C,  C.  C0  C2  C4  C6  C8  C:�C<  C>  C@  CB  CC�fCE�fCH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn�Cp�Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C��C��C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C��C�  C�  C��3C��3C�  C��C��C��C�  C�  C��C��C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C��3C�  C��C��C�  C��3C��3C��3C�  C��C��C��C�  C�  C��C�  C��C��C�  C�  C��C�  C�  C�  C��3C��3C��3C�  C��C��C�  C��3C�  C��C��C�  C��C��C��C�  C�  C�  C�  C��C�  C�  C��C��C�  C��3C��3C��3C�  C��C��3C��3D y�D  D� D��Dy�D��D� D  D� D��D� DfD�fDfDy�D�3Dy�D��D	� D
  D
� DfD�fDfD� D  D�fD  D� D  Dy�D  D� D��D� D  D� D  D� D  D�fD  D� D  D� D  D� DfD� D  D� D  Dy�D  D� D  Dy�D��D� DfD� DfD� D��D � D!fD!�fD"fD"� D"��D#�fD$  D$y�D$��D%y�D&  D&�fD'fD'� D(  D(�fD)fD)��D)��D*� D+  D+y�D+��D,�fD-fD-��D.  D.�fD.��D/� D/��D0y�D1fD1� D2  D2�fD2��D3y�D4  D4� D5  D5� D6  D6�fD6��D7� D8fD8��D9  D9y�D9��D:y�D:��D;� D;��D<y�D<��D=� D>  D>y�D?fD?� D@  D@� D@��DAy�DB  DB� DC  DC� DD  DD�fDD��DE� DFfDF�fDG  DGs3DG�3DHy�DIfDIy�DI��DJ� DKfDK� DL  DL�fDMfDMy�DM��DNy�DN��DO� DO��DPy�DP��DQy�DQ�3DRs3DS  DSy�DS��DTy�DU  DU� DVfDV�fDW  DW� DW��DXy�DX��DYy�DZ  DZ�fD[  D[�fD\  D\�fD]  D]�fD^  D^�fD_  D_� D_��D`y�D`��Da� DbfDb�fDcfDc�fDdfDd�fDefDe� Df  Df� Dg  Dg� Dh  Dh�fDifDi�fDj  Dj�fDkfDk� Dk��Dl� DmfDm� Dn  Dn�fDofDo�fDp  Dpy�Dp��Dq� DrfDr�fDs  Dsy�Dt  Dt� Dt��Du� Dv  Dv� Dv��Dw� Dw��Dy��D�#�D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @HQ�@��\@ʏ\A�A%G�AEG�AeG�A���A���A���A���A£�Aң�A��A��BQ�B	�RBQ�BQ�B!Q�B)Q�B1Q�B9Q�BAQ�BIQ�BQQ�BYQ�BaQ�BiQ�BqQ�ByQ�B���B���B���B��)B���B���B���B���B���B���B���B���B�u�B���B���B���B���BĨ�BȨ�B̨�BШ�B�u�Bب�Bܨ�B��B��B��B��B��B���B���B���C T{CT{CT{CT{CT{C
T{CT{CT{CT{CT{CT{CT{CT{CT{CT{CT{C T{C":�C$T{C&T{C(:�C*T{C,T{C.T{C0T{C2T{C4T{C6T{C8T{C:nC<T{C>T{C@T{CBT{CD:�CF:�CHT{CJT{CLT{CNT{CPT{CRT{CTT{CVT{CXT{CZT{C\T{C^T{C`T{CbT{CdT{CfT{ChT{CjT{ClT{CnnCpnCrT{CtT{CvT{CxT{CzT{C|T{C~T{C�*=C�*=C�*=C�*=C�*=C�7
C�7
C�*=C�*=C�7
C�7
C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�pC�pC�*=C�*=C�*=C�*=C�*=C�pC�pC�*=C�*=C�*=C�7
C�*=C�*=C�pC�pC�*=C�7
C�7
C�7
C�*=C�*=C�7
C�7
C�pC�pC�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�7
C�7
C�*=C�*=C�*=C�*=C�pC�*=C�*=C�*=C�*=C�*=C�pC�*=C�7
C�7
C�*=C�pC�pC�pC�*=C�7
C�7
C�7
C�*=C�*=C�7
C�*=C�7
C�7
C�*=C�*=C�7
C�*=C�*=C�*=C�pC�pC�pC�*=C�7
C�7
C�*=C�pC�*=C�7
C�7
C�*=C�7
C�C�C�7
C�*=C�*=C�*=C�*=C�7
C�*=C�*=C�7
C�C�C�*=C�pC�pC�pC�*=C�7
C�pD �D ��DD�D�D��D�D�DD�D�D�D�D��D�D��DRD��D	�D	�D
D
�D�D��D�D�DD��DD�DD��DD�D�D�DD�DD�DD��DD�DD�DD�D�D�DD�DD��DD�DD��D�D�D�D�D�D�D �D �D!�D!��D"�D"�D#�D#��D$D$��D%�D%��D&D&��D'�D'�D(D(��D)�D)��D*�D*�D+D+��D,�D,��D-�D-��D.D.��D/�D/�D0�D0��D1�D1�D2D2��D3�D3��D4D4�D5D5�D6D6��D7�D7�D8�D8��D9D9��D:�D:��D;�D;�D<�D<��D=�D=�D>D>��D?�D?�D@D@�DA�DA��DBDB�DCDC�DDDD��DE�DE�DF�DF��DGDG�RDHRDH��DI�DI��DJ�DJ�DK�DK�DLDL��DM�DM��DN�DN��DO�DO�DP�DP��DQ�DQ��DRRDR�RDSDS��DT�DT��DUDU�DV�DV��DWDW�DX�DX��DY�DY��DZDZ��D[D[��D\D\��D]D]��D^D^��D_D_�D`�D`��Da�Da�Db�Db��Dc�Dc��Dd�Dd��De�De�DfDf�DgDg�DhDh��Di�Di��DjDj��Dk�Dk�Dl�Dl�Dm�Dm�DnDn��Do�Do��DpDp��Dq�Dq�Dr�Dr��DsDs��DtDt�Du�Du�DvDv�Dw�Dw�Dx�Dy�D�.D��\1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�;dA�A�A�I�A�I�A�I�A�I�A�K�A�M�A�M�A�M�A�M�A�O�A�O�A�VA�K�A�(�A�&�A�A�A���A�z�A�?}A���A��yA�ȴA��jA��FA���A��A�|�A��A�|�A�hsA�O�A�G�A�;dA�9XA�I�A�A�A�5?A�7LA��A��#A���A��A���A��PA�x�A�x�A�t�A�|�A�z�A�VA�(�A���A��^A�x�A� �A�/A�?}A��TA��7A�=qA��!A��A�JA��RA�G�A���A���A���A��`A��A�l�A��FA�ZA��A�n�A��!A���A�~�A��-A�JA�p�A�^5A�VA��A��A��A���A�%A�hsA� �A�t�A�S�A�-A��9A�VA�M�A��A�33A��A�ȴA���A��;A��HA�oA��A��A���A��wA��A���A��RA�ĜA�n�A}��A{�#AzQ�Ay��Ax��Ax1'Aw��AwAv��At��Ar=qAo�mAn�uAm�Aj�Af��Ad �AbȴAa�A_�-A^�!A]�7A\��A\z�AZn�AX �AWAUt�ATQ�ASAQ33AO7LAL��AJ-AH��AH��AHVAG�AF��AE|�AEAC�7AAhsA@�DA@n�A@r�A?��A=�FA<z�A;�^A;XA;G�A;\)A:jA8��A8I�A7��A6��A6A5A5|�A4�A2�A0�RA0-A/��A/;dA.��A-��A+/A)�PA)"�A(Q�A&��A%|�A$�\A$ffA#33A"bA!A9XA|�A�jA~�AA~�A�AQ�AC�A�TA?}AĜA(�A��A9XA�-A�^A&�A�jAM�A�-AG�A�9AA�At�A
��A~�A%A~�AbAp�A%A�jAz�AA��AƨA��A�A 5?@�ff@�=q@��#@��@���@�1'@��F@�V@��`@�1'@��@�@�ȴ@��-@�33@��@�K�@���@�@�o@���@�r�@��@��T@�p�@��@�@�z�@�1'@�=q@��m@�K�@ޟ�@݁@���@�-@�A�@��y@�`B@Դ9@��@�dZ@��@ҸR@�7L@�ȴ@�%@�b@˝�@�;d@�@ʏ\@ɡ�@���@�bN@���@Ə\@�O�@�Z@��
@�l�@§�@�-@��@�I�@��m@��@�~�@�E�@��T@��9@�I�@���@�K�@�o@��H@�J@�O�@�Ĝ@�A�@�dZ@�o@��R@��`@��@�K�@�v�@��@��T@��/@��m@��@�dZ@�o@���@��^@�`B@��@���@�z�@�(�@�K�@�
=@���@��@�ȴ@���@���@���@�=q@��T@�x�@��@��@���@�bN@��F@�S�@�
=@��H@��+@�5?@��#@�p�@�&�@�V@�V@��@���@�j@� �@�
=@��+@�5?@��@�p�@�V@�&�@��@���@�z�@�9X@���@�@��H@��@��\@�ȴ@�
=@��y@���@��R@��!@��!@���@�v�@�=q@�@��@��@��T@��-@���@��7@�hs@��@�j@�(�@�b@��P@�l�@�;d@��H@��!@�~�@�ff@�=q@�J@���@�x�@�G�@�&�@��/@��@���@�S�@�;d@��!@�-@��@��@���@��@�hs@�7L@�/@���@��u@�Z@�9X@� �@�  @��w@�S�@�
=@��@��y@�ȴ@��\@�=q@���@�x�@�hs@�?}@���@���@�Ĝ@��j@��@�(�@���@���@�dZ@�"�@���@���@�ff@�5?@��@��@���@�?}@�&�@��@��@���@��`@��j@��u@�z�@�b@��
@��F@���@���@��@�l�@��H@���@���@�v�@�M�@�J@���@��h@�hs@�7L@�&�@��@���@�"h@t�@l2�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�;dA�A�A�I�A�I�A�I�A�I�A�K�A�M�A�M�A�M�A�M�A�O�A�O�A�VA�K�A�(�A�&�A�A�A���A�z�A�?}A���A��yA�ȴA��jA��FA���A��A�|�A��A�|�A�hsA�O�A�G�A�;dA�9XA�I�A�A�A�5?A�7LA��A��#A���A��A���A��PA�x�A�x�A�t�A�|�A�z�A�VA�(�A���A��^A�x�A� �A�/A�?}A��TA��7A�=qA��!A��A�JA��RA�G�A���A���A���A��`A��A�l�A��FA�ZA��A�n�A��!A���A�~�A��-A�JA�p�A�^5A�VA��A��A��A���A�%A�hsA� �A�t�A�S�A�-A��9A�VA�M�A��A�33A��A�ȴA���A��;A��HA�oA��A��A���A��wA��A���A��RA�ĜA�n�A}��A{�#AzQ�Ay��Ax��Ax1'Aw��AwAv��At��Ar=qAo�mAn�uAm�Aj�Af��Ad �AbȴAa�A_�-A^�!A]�7A\��A\z�AZn�AX �AWAUt�ATQ�ASAQ33AO7LAL��AJ-AH��AH��AHVAG�AF��AE|�AEAC�7AAhsA@�DA@n�A@r�A?��A=�FA<z�A;�^A;XA;G�A;\)A:jA8��A8I�A7��A6��A6A5A5|�A4�A2�A0�RA0-A/��A/;dA.��A-��A+/A)�PA)"�A(Q�A&��A%|�A$�\A$ffA#33A"bA!A9XA|�A�jA~�AA~�A�AQ�AC�A�TA?}AĜA(�A��A9XA�-A�^A&�A�jAM�A�-AG�A�9AA�At�A
��A~�A%A~�AbAp�A%A�jAz�AA��AƨA��A�A 5?@�ff@�=q@��#@��@���@�1'@��F@�V@��`@�1'@��@�@�ȴ@��-@�33@��@�K�@���@�@�o@���@�r�@��@��T@�p�@��@�@�z�@�1'@�=q@��m@�K�@ޟ�@݁@���@�-@�A�@��y@�`B@Դ9@��@�dZ@��@ҸR@�7L@�ȴ@�%@�b@˝�@�;d@�@ʏ\@ɡ�@���@�bN@���@Ə\@�O�@�Z@��
@�l�@§�@�-@��@�I�@��m@��@�~�@�E�@��T@��9@�I�@���@�K�@�o@��H@�J@�O�@�Ĝ@�A�@�dZ@�o@��R@��`@��@�K�@�v�@��@��T@��/@��m@��@�dZ@�o@���@��^@�`B@��@���@�z�@�(�@�K�@�
=@���@��@�ȴ@���@���@���@�=q@��T@�x�@��@��@���@�bN@��F@�S�@�
=@��H@��+@�5?@��#@�p�@�&�@�V@�V@��@���@�j@� �@�
=@��+@�5?@��@�p�@�V@�&�@��@���@�z�@�9X@���@�@��H@��@��\@�ȴ@�
=@��y@���@��R@��!@��!@���@�v�@�=q@�@��@��@��T@��-@���@��7@�hs@��@�j@�(�@�b@��P@�l�@�;d@��H@��!@�~�@�ff@�=q@�J@���@�x�@�G�@�&�@��/@��@���@�S�@�;d@��!@�-@��@��@���@��@�hs@�7L@�/@���@��u@�Z@�9X@� �@�  @��w@�S�@�
=@��@��y@�ȴ@��\@�=q@���@�x�@�hs@�?}@���@���@�Ĝ@��j@��@�(�@���@���@�dZ@�"�@���@���@�ff@�5?@��@��@���@�?}@�&�@��@��@���@��`@��j@��u@�z�@�b@��
@��F@���@���@��@�l�@��H@���@���@�v�@�M�@�J@���@��h@�hs@�7L@�&�@��@���@�"h@t�@l2�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B5?B5?B49B49B49B49B5?B49B49B49B33B6FB7LB>wBR�B\)Bm�B� B�B�7B�JB�JB�1B�+B�7B�PB�\B�\B�JB�PB�bB�uB�oB�\B�VB�JB�JB�hB�bB�hB��B��B�oB�\B�PB�PB�JB�=B�DB�DB�bB�uB�{B��B��B��B��B�3B�-B�'B�-B�3B�-B�-B�!B�B�B�B��B��B��B��B��B��B�hB�BjBdZB`BB_;BYBXBW
BP�BO�BM�BG�B?}B7LB(�B�B��B�B��B�jB�'B��B��B�hB�B{�By�Bx�Bv�Be`BK�B=qB#�BPB
��B
�B
�#B
�B
ĜB
��B
�1B
q�B
e`B
\)B
XB
S�B
O�B
M�B
M�B
G�B
8RB
&�B
{B
	7B	��B	�B	�HB	�B	��B	ÖB	�dB	�9B	�B	��B	��B	��B	�%B	|�B	o�B	e`B	ZB	L�B	6FB	�B	VB	�B	�B	�B	�B	�B	oB	VB	%B��B�B�B�B�B�sB�;B�5B�)B�/B�BB�B�mB�`B�TB�BB�5B�/B�#B�
B��B��B��B��BȴBŢB��B�^B�FB�3B�B��B��B��B��B��B��B��B�hB�PB�DB�=B�1B�B�B�B~�B~�B~�B~�B|�B}�B|�B|�B~�B~�B~�B|�B|�B{�By�By�Bw�Bs�Bs�Bt�Bt�Bs�Bs�Bs�Br�Bq�Bp�Bp�Bo�Bn�Bn�Bn�Bo�Bo�Bn�Bo�Bn�Bn�Bm�Bm�Bm�Bl�Bl�Bk�BjBiyBjBiyBl�Bo�Bp�Bp�Bq�Bq�Bq�Br�Bs�Bs�Br�Br�Bp�Br�Bw�Bz�B{�B~�B~�B�B�1B�JB�hB�oB�oB�oB�uB�{B��B��B��B��B��B��B��B��B��B��B��B��B�B�'B�FB�LB�RB�dB�qB�}BŢBƨBɺB��B��B��B��B�
B�B�)B�5B�;B�TB�fB�sB�B�B��B��B��B��B��B	B	B	B	1B	\B	{B	�B	�B	�B	�B	�B	�B	�B	!�B	#�B	,B	-B	.B	/B	2-B	7LB	;dB	<jB	<jB	<jB	?}B	A�B	D�B	F�B	H�B	L�B	L�B	L�B	N�B	O�B	O�B	R�B	VB	XB	YB	YB	ZB	^5B	bNB	dZB	k�B	n�B	p�B	p�B	r�B	t�B	v�B	{�B	~�B	� B	�B	�B	�B	�+B	�+B	�+B	�JB	�hB	�hB	�hB	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�'B	�'B	�-B	�-B	�-B	�3B	�3B	�9B	�9B	�9B	�3B	�9B	�9B	�9B	�9B	�FB	�XB	�XB	�^B	�dB	�wB	�wB	�}B	��B	��B	ĜB	ƨB	ǮB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�#B	�)B	�;B	�BB	�HB	�NB	�TB	�ZB	�`B	�fB	�mB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
+B
	7B
	�B
B
*e2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 B5?B5?B49B49B49B49B5?B49B49B49B33B6FB7LB>wBR�B\)Bm�B� B�B�7B�JB�JB�1B�+B�7B�PB�\B�\B�JB�PB�bB�uB�oB�\B�VB�JB�JB�hB�bB�hB��B��B�oB�\B�PB�PB�JB�=B�DB�DB�bB�uB�{B��B��B��B��B�3B�-B�'B�-B�3B�-B�-B�!B�B�B�B��B��B��B��B��B��B�hB�BjBdZB`BB_;BYBXBW
BP�BO�BM�BG�B?}B7LB(�B�B��B�B��B�jB�'B��B��B�hB�B{�By�Bx�Bv�Be`BK�B=qB#�BPB
��B
�B
�#B
�B
ĜB
��B
�1B
q�B
e`B
\)B
XB
S�B
O�B
M�B
M�B
G�B
8RB
&�B
{B
	7B	��B	�B	�HB	�B	��B	ÖB	�dB	�9B	�B	��B	��B	��B	�%B	|�B	o�B	e`B	ZB	L�B	6FB	�B	VB	�B	�B	�B	�B	�B	oB	VB	%B��B�B�B�B�B�sB�;B�5B�)B�/B�BB�B�mB�`B�TB�BB�5B�/B�#B�
B��B��B��B��BȴBŢB��B�^B�FB�3B�B��B��B��B��B��B��B��B�hB�PB�DB�=B�1B�B�B�B~�B~�B~�B~�B|�B}�B|�B|�B~�B~�B~�B|�B|�B{�By�By�Bw�Bs�Bs�Bt�Bt�Bs�Bs�Bs�Br�Bq�Bp�Bp�Bo�Bn�Bn�Bn�Bo�Bo�Bn�Bo�Bn�Bn�Bm�Bm�Bm�Bl�Bl�Bk�BjBiyBjBiyBl�Bo�Bp�Bp�Bq�Bq�Bq�Br�Bs�Bs�Br�Br�Bp�Br�Bw�Bz�B{�B~�B~�B�B�1B�JB�hB�oB�oB�oB�uB�{B��B��B��B��B��B��B��B��B��B��B��B��B�B�'B�FB�LB�RB�dB�qB�}BŢBƨBɺB��B��B��B��B�
B�B�)B�5B�;B�TB�fB�sB�B�B��B��B��B��B��B	B	B	B	1B	\B	{B	�B	�B	�B	�B	�B	�B	�B	!�B	#�B	,B	-B	.B	/B	2-B	7LB	;dB	<jB	<jB	<jB	?}B	A�B	D�B	F�B	H�B	L�B	L�B	L�B	N�B	O�B	O�B	R�B	VB	XB	YB	YB	ZB	^5B	bNB	dZB	k�B	n�B	p�B	p�B	r�B	t�B	v�B	{�B	~�B	� B	�B	�B	�B	�+B	�+B	�+B	�JB	�hB	�hB	�hB	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�'B	�'B	�-B	�-B	�-B	�3B	�3B	�9B	�9B	�9B	�3B	�9B	�9B	�9B	�9B	�FB	�XB	�XB	�^B	�dB	�wB	�wB	�}B	��B	��B	ĜB	ƨB	ǮB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�#B	�)B	�;B	�BB	�HB	�NB	�TB	�ZB	�`B	�fB	�mB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
+B
	7B
	�B
B
*e2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.33 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005191754                              AO  ARCAADJP                                                                    20181005191754    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005191754  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005191754  QCF$                G�O�G�O�G�O�8000            