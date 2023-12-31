CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:06:03Z creation      
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20181005190603  20181005190603  5904952 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6431                            2B  A   APEX                            7466                            062512                          846 @��)J�V�1   @��)�-��@1�+I��c��+J1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �A   A   A   @&ff@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B'��B0  B8  B@  BH  BP  BX  B`  Bh  Bo��Bw��B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,�C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\�C^�C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C��3C�  C��C��C��C�  C��3C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C��3C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C��3C��3C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  D   D � D  D� D  D� D  D� D��D� D  D� D  D� D  D� D  D� D	fD	�fD
  D
� D
��D� D  D� D  Dy�D  D�fDfD�fDfD�fDfD� D  D� D  Dy�D  D� D  Dy�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD�fD   D y�D!  D!� D"  D"�fD#  D#� D$  D$y�D%  D%� D&  D&� D'  D'� D(fD(� D)  D)y�D*  D*� D+  D+� D,fD,� D-  D-� D.fD.� D.��D/y�D/��D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6fD6�fD6��D7y�D8  D8� D9  D9� D:  D:� D:��D;� D<  D<� D=  D=� D>fD>� D?  D?� D@  D@� DA  DA� DBfDB� DC  DCy�DC��DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJfDJ� DJ��DKy�DL  DL� DMfDM� DM��DNy�DO  DOy�DP  DP� DQfDQ� DQ��DR� DSfDS�fDTfDT� DT��DUy�DV  DV� DW  DWy�DX  DX� DY  DY�fDZ  DZ� DZ��D[� D\fD\�fD]  D]� D^  D^� D^��D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� De��Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk�fDlfDl� Dm  Dmy�Dn  Dn� Do  Do� Dp  Dp� DqfDq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dw��Dy��D�-�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @;�@��\@ʏ\AG�A%G�AEG�AeG�A���A���A���A���A£�Aң�A��A��BQ�B	Q�BQ�BQ�B!Q�B(�B1Q�B9Q�BAQ�BIQ�BQQ�BYQ�BaQ�BiQ�Bp�Bx�B���B���B���B���B�u�B���B���B���B���B���B���B��)B���B���B���B���B���BĨ�BȨ�B̨�BШ�BԨ�Bب�Bܨ�B��B��)B��B��B��B���B���B���C T{CT{CT{CT{CT{C
T{CT{CT{CT{CT{CT{CT{CT{CT{CT{CT{C T{C"T{C$T{C&T{C(T{C*T{C,nC.T{C0T{C2T{C4T{C6T{C8T{C:T{C<T{C>T{C@T{CBT{CDT{CFT{CHT{CJT{CLT{CNT{CPT{CRT{CTT{CVT{CXT{CZT{C\nC^nC`T{CbT{CdT{CfT{ChT{CjT{ClT{CnT{CpT{CrT{CtT{CvT{CxT{CzT{C|T{C~T{C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�pC�*=C�*=C�*=C�*=C�pC�*=C�7
C�7
C�7
C�*=C�pC�*=C�*=C�pC�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�7
C�*=C�*=C�*=C�*=C�pC�*=C�*=C�7
C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�pC�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�7
C�*=C�*=C�pC�pC�*=C�7
C�7
C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�pC�pC�*=C�*=C�*=C�*=C�*=C�*=C�pC�pC�*=C�*=C�*=D D �DD�DD�DD�D�D�DD�DD�DD�DD�D	�D	��D
D
�D�D�DD�DD��DD��D�D��D�D��D�D�DD�DD��DD�DD��DD�DD�DD�DD�DD�DD�DD�DD�DD�D�D��D D ��D!D!�D"D"��D#D#�D$D$��D%D%�D&D&�D'D'�D(�D(�D)D)��D*D*�D+D+�D,�D,�D-D-�D.�D.�D/�D/��D0�D0�D1D1�D2D2�D3D3�D4D4�D5D5�D6�D6��D7�D7��D8D8�D9D9�D:D:�D;�D;�D<D<�D=D=�D>�D>�D?D?�D@D@�DADA�DB�DB�DCDC��DD�DD�DEDE�DFDF�DGDG�DHDH�DIDI�DJ�DJ�DK�DK��DLDL�DM�DM�DN�DN��DODO��DPDP�DQ�DQ�DR�DR�DS�DS��DT�DT�DU�DU��DVDV�DWDW��DXDX�DYDY��DZDZ�D[�D[�D\�D\��D]D]�D^D^�D_�D_�D`D`�DaDa�DbDb�DcDc�DdDd�DeDe�Df�Df�DgDg�DhDh�DiDi�DjDj�DkDk��Dl�Dl�DmDm��DnDn�DoDo�DpDp�Dq�Dq�DrDr�DsDs�DtDt�DuDu�DvDv�DwDw�Dx�Dy��D�8R111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�A�A�A�A�A�A�A�  A���A˴9A�M�AɋDAɍPA�l�A�ZA�ƨA���A�bA��AđhA�{A�;dA�A��A��#A�r�A�%A�ffA�-A�
=A��A��-A���A�I�A�%A��-A���A���A���A���A�`BA���A��
A�ĜA�x�A��uA�&�A��A�jA���A�x�A��7A��A�A�A�A�dZA���A�oA�p�A�VA�ƨA��-A���A���A��DA�(�A�VA�ffA�M�A��FA��A�~�A�JA�;dA���A���A�\)A��A�+A���A�p�A���A��A�33A��HA�+A�1'A�VA��-A���A���A���A��A��wA���A�(�A�  A}C�AxffAv�9AtVAp�jAl��Aj1AiS�Aex�A`$�A\�\A\A�AZI�AW7LAU;dATE�AR�AQXAP��AOG�AM?}ALE�AKS�AK%AD�A>�A<n�A:r�A9�A7�A5�A2��A1�^A01'A/�7A-�;A,bNA+�A+�#A+�PA+?}A*ĜA)��A(�A'�PA'K�A&�9A&  A%��A$��A$bA#&�A"��A �RAO�A�A��A�RA�mA��A7LA��A�RAJA�AA5?AAȴA~�AjA�AA��AJA;dA��A��A�PA;dAVA
�\A
��A
n�A	?}AI�Al�A33A�RA�A�AJAz�AjA�Al�A ��A ^5A bNA ZA Q�A j@�ȴ@��j@�@�O�@��@��;@�l�@��H@���@��\@�5?@�x�@��m@�j@�P@�V@�hs@�V@�j@�r�@��@�1@��m@�K�@���@��@�r�@웦@�j@�@@��@�"�@�j@�1'@�\@�O�@�%@��/@蛦@�  @�ff@�&�@䛦@�1'@���@��m@�F@���@⟾@�E�@��@�O�@��@�(�@ߝ�@��y@އ+@��T@�7L@ۅ@�~�@���@ڰ!@ٲ-@�A�@�(�@ج@���@���@��@�%@���@�Ĝ@� �@�\)@�33@�1'@ؓu@�b@�C�@�"�@�ff@��@�bN@�  @Ӿw@Ұ!@�^5@�$�@���@��T@ѩ�@с@�hs@��@�  @��y@�n�@�$�@́@�V@�Ĝ@�Ĝ@���@�Z@��@��;@˶F@˥�@˥�@˅@�l�@�;d@�@ʰ!@�J@�x�@�%@ȓu@�9X@�1@���@��@�n�@���@��T@Ł@�7L@ě�@�(�@���@�l�@§�@�V@���@��F@���@�ff@��\@���@�ff@��@��@��@�Z@���@�@�~�@�-@���@�7L@���@��m@�dZ@��H@�-@��@���@�7L@�1@���@�+@��R@�~�@�E�@�=q@�$�@��@��9@�9X@�(�@��@���@��@�v�@��@�$�@��#@���@��h@�A�@�1@�ƨ@���@�|�@�dZ@�K�@�K�@�K�@�dZ@�o@��R@�M�@�$�@�J@�p�@��`@��9@��@���@��D@�%@�&�@��@��/@��@�@��!@�n�@�5?@�5?@�-@��@��@�{@���@�p�@�G�@���@���@�I�@�1@��m@���@��@�S�@��@�~�@�@�p�@�X@���@�I�@�b@���@�33@��R@�ȴ@��!@�V@��T@��-@�%@���@��;@��P@�C�@�"�@�@��@���@�~�@�^5@�M�@��@��T@�@���@��@��@�V@���@��9@�r�@��
@���@���@�|�@�dZ@�C�@��@��@���@�5?@��@��h@��@�hs@�G�@��@��`@�Ĝ@�Ĝ@��@�r�@�A�@� �@��;@��w@��@�o@�ȴ@�n�@�5?@��@��T@�?}@�"h111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�A�A�A�A�A�A�A�  A���A˴9A�M�AɋDAɍPA�l�A�ZA�ƨA���A�bA��AđhA�{A�;dA�A��A��#A�r�A�%A�ffA�-A�
=A��A��-A���A�I�A�%A��-A���A���A���A���A�`BA���A��
A�ĜA�x�A��uA�&�A��A�jA���A�x�A��7A��A�A�A�A�dZA���A�oA�p�A�VA�ƨA��-A���A���A��DA�(�A�VA�ffA�M�A��FA��A�~�A�JA�;dA���A���A�\)A��A�+A���A�p�A���A��A�33A��HA�+A�1'A�VA��-A���A���A���A��A��wA���A�(�A�  A}C�AxffAv�9AtVAp�jAl��Aj1AiS�Aex�A`$�A\�\A\A�AZI�AW7LAU;dATE�AR�AQXAP��AOG�AM?}ALE�AKS�AK%AD�A>�A<n�A:r�A9�A7�A5�A2��A1�^A01'A/�7A-�;A,bNA+�A+�#A+�PA+?}A*ĜA)��A(�A'�PA'K�A&�9A&  A%��A$��A$bA#&�A"��A �RAO�A�A��A�RA�mA��A7LA��A�RAJA�AA5?AAȴA~�AjA�AA��AJA;dA��A��A�PA;dAVA
�\A
��A
n�A	?}AI�Al�A33A�RA�A�AJAz�AjA�Al�A ��A ^5A bNA ZA Q�A j@�ȴ@��j@�@�O�@��@��;@�l�@��H@���@��\@�5?@�x�@��m@�j@�P@�V@�hs@�V@�j@�r�@��@�1@��m@�K�@���@��@�r�@웦@�j@�@@��@�"�@�j@�1'@�\@�O�@�%@��/@蛦@�  @�ff@�&�@䛦@�1'@���@��m@�F@���@⟾@�E�@��@�O�@��@�(�@ߝ�@��y@އ+@��T@�7L@ۅ@�~�@���@ڰ!@ٲ-@�A�@�(�@ج@���@���@��@�%@���@�Ĝ@� �@�\)@�33@�1'@ؓu@�b@�C�@�"�@�ff@��@�bN@�  @Ӿw@Ұ!@�^5@�$�@���@��T@ѩ�@с@�hs@��@�  @��y@�n�@�$�@́@�V@�Ĝ@�Ĝ@���@�Z@��@��;@˶F@˥�@˥�@˅@�l�@�;d@�@ʰ!@�J@�x�@�%@ȓu@�9X@�1@���@��@�n�@���@��T@Ł@�7L@ě�@�(�@���@�l�@§�@�V@���@��F@���@�ff@��\@���@�ff@��@��@��@�Z@���@�@�~�@�-@���@�7L@���@��m@�dZ@��H@�-@��@���@�7L@�1@���@�+@��R@�~�@�E�@�=q@�$�@��@��9@�9X@�(�@��@���@��@�v�@��@�$�@��#@���@��h@�A�@�1@�ƨ@���@�|�@�dZ@�K�@�K�@�K�@�dZ@�o@��R@�M�@�$�@�J@�p�@��`@��9@��@���@��D@�%@�&�@��@��/@��@�@��!@�n�@�5?@�5?@�-@��@��@�{@���@�p�@�G�@���@���@�I�@�1@��m@���@��@�S�@��@�~�@�@�p�@�X@���@�I�@�b@���@�33@��R@�ȴ@��!@�V@��T@��-@�%@���@��;@��P@�C�@�"�@�@��@���@�~�@�^5@�M�@��@��T@�@���@��@��@�V@���@��9@�r�@��
@���@���@�|�@�dZ@�C�@��@��@���@�5?@��@��h@��@�hs@�G�@��@��`@�Ĝ@�Ĝ@��@�r�@�A�@� �@��;@��w@��@�o@�ȴ@�n�@�5?@��@��T@�?}@�"h111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BǮBȴBǮBǮBȴBȴBɺB��B�B�B	e`B	�B	�!B	�wB
%B
Q�B
� B
��B
��B
�?B
��B
�B
��B
��B
��B\B�B,B5?B<jBA�BF�BJ�BQ�BT�BT�BS�BS�BS�BS�BQ�BT�Bt�B�bB��BÖB�/B�B	7B\B�B"�BB�B�mB�B�ZB�`B�sB��B��B��BɺBɺBɺBɺBȴB�jB�RB�LB�XB�^B�?B�B��B��B��B�=B� Bm�BR�BC�B7LB�B{B%B
�`B
�B
}�B
?}B
�B
oB
{B
I�B
0!B
  B	�;B	B	��B	��B	�JB	w�B	aHB	P�B	J�B	49B	�B	%B	B��B�B�B�B�B�B�B��B	VB	�B	�B	"�B	%B�TB��BǮBȴBĜB�wB�?B�9B�3B�-B�?BĜB��B��B��B�/B�)B�HB�5B�#B�B�
B�B�5B�B�sB�NB�5B��BÖB�dB�RB�RB�dB�wBȴB�;B�yB�`B�B�B��B		7B	oB	�B	�B	DB�B�;B�sB�ZB�#B�B�B�B�)B�/B�`B�yB�HB�TB�mB�yB��B��B	B��B�B�yB�mB�mB�B��B��B��B	%B	PB	VB	VB	VB	PB	JB	JB	JB	PB	PB	JB	JB	JB	DB	DB	DB	PB	VB	\B	hB	�B	$�B	'�B	,B	/B	9XB	J�B	VB	]/B	aHB	k�B	~�B	�VB	�PB	�%B	�B	�=B	�PB	�\B	�bB	�bB	�bB	�bB	�oB	�uB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�{B	�uB	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�3B	�9B	�?B	�LB	�^B	B	ƨB	ȴB	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�)B	�)B	�;B	�;B	�;B	�;B	�;B	�;B	�;B	�;B	�;B	�;B	�;B	�;B	�;B	�5B	�/B	�/B	�/B	�;B	�HB	�NB	�NB	�NB	�NB	�NB	�NB	�NB	�NB	�NB	�NB	�TB	�ZB	�;B	�5B	�HB	�TB	�ZB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
%B
%B
%B
%B
%B
+B
+B
+B
+B
%B
B
B
B
B
B
B
B
+B
+B
%B
%B
B
B
B
B
B
%B
%B
+B
+B
1B
1B
1B
1B
	7B
	7B

=B
DB
JB
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
uB
uB
uB
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
)B
#222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  BǮBȴBǮBǮBȴBȴBɺB��B�B�B	e`B	�B	�!B	�wB
%B
Q�B
� B
��B
��B
�?B
��B
�B
��B
��B
��B\B�B,B5?B<jBA�BF�BJ�BQ�BT�BT�BS�BS�BS�BS�BQ�BT�Bt�B�bB��BÖB�/B�B	7B\B�B"�BB�B�mB�B�ZB�`B�sB��B��B��BɺBɺBɺBɺBȴB�jB�RB�LB�XB�^B�?B�B��B��B��B�=B� Bm�BR�BC�B7LB�B{B%B
�`B
�B
}�B
?}B
�B
oB
{B
I�B
0!B
  B	�;B	B	��B	��B	�JB	w�B	aHB	P�B	J�B	49B	�B	%B	B��B�B�B�B�B�B�B��B	VB	�B	�B	"�B	%B�TB��BǮBȴBĜB�wB�?B�9B�3B�-B�?BĜB��B��B��B�/B�)B�HB�5B�#B�B�
B�B�5B�B�sB�NB�5B��BÖB�dB�RB�RB�dB�wBȴB�;B�yB�`B�B�B��B		7B	oB	�B	�B	DB�B�;B�sB�ZB�#B�B�B�B�)B�/B�`B�yB�HB�TB�mB�yB��B��B	B��B�B�yB�mB�mB�B��B��B��B	%B	PB	VB	VB	VB	PB	JB	JB	JB	PB	PB	JB	JB	JB	DB	DB	DB	PB	VB	\B	hB	�B	$�B	'�B	,B	/B	9XB	J�B	VB	]/B	aHB	k�B	~�B	�VB	�PB	�%B	�B	�=B	�PB	�\B	�bB	�bB	�bB	�bB	�oB	�uB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�{B	�uB	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�3B	�9B	�?B	�LB	�^B	B	ƨB	ȴB	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�)B	�)B	�;B	�;B	�;B	�;B	�;B	�;B	�;B	�;B	�;B	�;B	�;B	�;B	�;B	�5B	�/B	�/B	�/B	�;B	�HB	�NB	�NB	�NB	�NB	�NB	�NB	�NB	�NB	�NB	�NB	�TB	�ZB	�;B	�5B	�HB	�TB	�ZB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
%B
%B
%B
%B
%B
+B
+B
+B
+B
%B
B
B
B
B
B
B
B
+B
+B
%B
%B
B
B
B
B
B
%B
%B
+B
+B
1B
1B
1B
1B
	7B
	7B

=B
DB
JB
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
uB
uB
uB
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
)B
#222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.33 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005190603                              AO  ARCAADJP                                                                    20181005190603    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005190603  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005190603  QCF$                G�O�G�O�G�O�8000            