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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20181005191754  20181005191754  5904954 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               A   AO  6557                            2B  A   APEX                            7468                            062512                          846 @��d�v<1   @��e33E�@5S�E����dI�^1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      A   A   A   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C�fC
  C  C  C  C�fC  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C1�fC3�fC6  C8  C:  C<  C>  C@  CB  CC�fCE�fCH  CJ  CK�fCM�fCP  CR�CT�CV  CX  CZ  C\  C^  C`  Cb�Cd  Ce�fCh  Cj  Cl  Cn  Co�fCr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C��C�  C�  C�  C�  C�  C��3C��3C�  C��C�  C�  C��C�  C�  C�  C�  C��3C�  C�  C�  C�  C��C��C��3C��3C�  C�  C��3C�  C�  C�  C��3C��3C��3C�  C�  C�  C�  C��3C�  C�  C��C�  C�  C�  C�  C�  C��3C�  C��C�  C�  C�  C�  C��3C��3C�  C��C�  C�  C�  C��3C��3C��3C��3C�  C��3C��3C��3C�  C�  C��3C�  C�  C�  C��C�  C��3C��3C��3C��3C�  C�  C��3C�  C��C��3D � DfD�fD  Dy�D  D�fD  D�fD�D�fD  D�fDfD� D  D� D	  D	�fD
  D
� D  D� DfD� D  D� DfDy�D��Dy�D��D�fD�D� D  D� DfD�fD  D��D  D�fD��D� D  D� DfD�fD  Dy�D  D� D  D�fD�D� DfD�fD  D� D  D� D   D � D ��D!� D!��D"� D#  D#�fD#��D$�fD$��D%�fD&  D&� D'  D'y�D'�3D(�fD)  D)� D*  D*� D+  D+� D,  D,�fD-  D-y�D-�3D.� D/  D/y�D0  D0y�D1  D1�fD2fD2� D3  D3�fD4  D4� D5  D5� D5��D6�fD7  D7�fD7��D8� D9  D9� D:  D:� D;  D;� D<  D<�fD=  D=�fD>  D>y�D>��D?y�D@  D@� D@��DA� DB  DB� DC  DC�fDD  DD� DE  DE� DFfDF� DG  DG� DG��DHy�DH��DI� DJ  DJy�DJ��DK� DL  DL� DMfDM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS�fDT  DT�fDUfDU� DV  DV� DW  DW�fDW��DX�fDY  DY�fDZ  DZ� DZ��D[� D\fD\�fD\��D]� D^fD^� D_  D_y�D`fD`� D`��Da� Db  Dby�Db��Dcy�Dd  Dd� DefDe� De��Df� DgfDg� Dh  Dh� Di  Di� Di��Dj� Dk  Dk� DlfDly�Dl��Dm� Dm��Dn� Dn��Do� Dp  Dp�fDq  Dq� Dq�3Dr�fDsfDs� DtfDt� Du  Du� Du��Dv�fDw  Dwy�Dw� Dy��D�9�D� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�@ʏ\AG�A%G�AEG�AeG�A���A���A���A���A£�Aң�A��A��BQ�B	Q�BQ�BQ�B!Q�B)Q�B1Q�B9Q�BAQ�BIQ�BQQ�BYQ�BaQ�BiQ�BqQ�ByQ�B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���BĨ�BȨ�B̨�BШ�BԨ�Bب�Bܨ�B��B��B��B��B��B���B���B���C T{CT{CT{CT{C:�C
T{CT{CT{CT{C:�CT{CT{CT{CT{CT{CT{C T{C"T{C$T{C&T{C(T{C*T{C,T{C.T{C0T{C2:�C4:�C6T{C8T{C:T{C<T{C>T{C@T{CBT{CD:�CF:�CHT{CJT{CL:�CN:�CPT{CRnCTnCVT{CXT{CZT{C\T{C^T{C`T{CbnCdT{Cf:�ChT{CjT{ClT{CnT{Cp:�CrT{CtT{CvT{CxT{CzT{C|T{C~T{C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�pC�*=C�*=C�pC�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�7
C�7
C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�pC�*=C�7
C�*=C�*=C�*=C�*=C�*=C�pC�pC�*=C�7
C�*=C�*=C�7
C�*=C�*=C�*=C�*=C�pC�*=C�*=C�*=C�*=C�7
C�7
C�pC�pC�*=C�*=C�pC�*=C�*=C�*=C�pC�pC�pC�*=C�*=C�*=C�*=C�pC�*=C�*=C�7
C�*=C�*=C�*=C�*=C�*=C�pC�*=C�7
C�*=C�*=C�*=C�*=C�pC�pC�*=C�7
C�*=C�*=C�*=C�pC�pC�pC�pC�*=C�pC�pC�pC�*=C�*=C�pC�*=C�*=C�*=C�7
C�*=C�pC�pC�pC�pC�*=C�*=C�pC�*=C�7
D �D �D�D��DD��DD��DD��D!�D��DD��D�D�DD�D	D	��D
D
�DD�D�D�DD�D�D��D�D��D�D��D!�D�DD�D�D��DD��DD��D�D�DD�D�D��DD��DD�DD��D!�D�D�D��DD�DD�D D �D!�D!�D"�D"�D#D#��D$�D$��D%�D%��D&D&�D'D'��D(RD(��D)D)�D*D*�D+D+�D,D,��D-D-��D.RD.�D/D/��D0D0��D1D1��D2�D2�D3D3��D4D4�D5D5�D6�D6��D7D7��D8�D8�D9D9�D:D:�D;D;�D<D<��D=D=��D>D>��D?�D?��D@D@�DA�DA�DBDB�DCDC��DDDD�DEDE�DF�DF�DGDG�DH�DH��DI�DI�DJDJ��DK�DK�DLDL�DM�DM�DNDN�DODO�DPDP�DQDQ�DRDR�DSDS��DTDT��DU�DU�DVDV�DWDW��DX�DX��DYDY��DZDZ�D[�D[�D\�D\��D]�D]�D^�D^�D_D_��D`�D`�Da�Da�DbDb��Dc�Dc��DdDd�De�De�Df�Df�Dg�Dg�DhDh�DiDi�Dj�Dj�DkDk�Dl�Dl��Dm�Dm�Dn�Dn�Do�Do�DpDp��DqDq�DrRDr��Ds�Ds�Dt�Dt�DuDu�Dv�Dv��DwDw��Dw�Dy�D�D{D��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��A��yA��A��`A���A���A��hA�ffA�&�A���A��
A��hA�C�A��`A��A�x�A�v�A�r�A�n�A�jA�ffA�jA��DA��hA��7A��+A�z�A�K�A�7LA�VA��
A�ƨA�x�A�A��A��A��HA���A�r�A�\)A�XA�^5A�XA�M�A�G�A�G�A�E�A�?}A�(�A��A��A�oA�VA�JA�JA���A��A���A���A�K�A��`A�~�A�~�A�9XA�XA���A��jA��!A�M�A�~�A�VA���A�+A��A���A���A��!A���A�A��9A��uA��jA��#A�bNA�  A�{A�M�A�ƨA�Q�A��HA�;dA��uA��^A�O�A�A��DA�
=A��HA�~�A�1'A�?}A���A�-A�5?A��RA�ffA�M�A�^5A�K�A�9XA�"�A���A���A�^5A���A|�Ax�Aw33Av�yAudZAsdZAq/AoS�Al�HAk`BAj��AiG�Ae��Ac�FAb{A`v�A_�PA^v�A[XAY��AYS�AX�AW�;AU�7AS"�AR$�AQ�AQt�AQ&�AP�DAPM�AO�-ANVAH��AG�AGAF�AE��AE"�ADbNAC�TAC/AA��A@��A?�A??}A>n�A<�RA:�HA:1'A9/A7x�A6E�A6 �A5��A4��A2�HA2�A1��A0^5A/K�A.��A-��A,^5A*��A*�DA*bA)S�A'�;A%�A#��A"�A!33A A�A�A�\A�TA�A/A�\A�A9XA�AI�A�A��A�+A�mA�7A�AQ�A�A"�AE�A�7Ap�A��Az�A�A�FA;dAA�jA-A
��A
z�A
9XA	��A	G�Ar�A(�A�A�AM�A�A�\A7LA�A��A bN@��@���@���@�1@��`@���@��@��-@��@�j@��@�\@��D@�\)@�V@�G�@��/@�Ĝ@�@�@�@�j@�@���@��@�@�v�@�-@���@ߍP@��@��@�^5@��@�K�@�
=@�ȴ@�z�@�@�V@�bN@϶F@���@�Z@�|�@�p�@�r�@���@���@�S�@�J@š�@�%@å�@�@�G�@��`@���@��F@��@��^@�G�@���@�A�@���@���@��@�Ĝ@�b@���@�l�@�;d@���@��@���@�X@�/@���@�9X@��!@�E�@�=q@�5?@���@�?}@��D@��m@�dZ@�E�@�=q@��/@��@�Z@�A�@��@��P@�33@�@�^5@�@��#@��^@���@�p�@�%@��@�r�@��;@��w@��P@�;d@��+@�M�@�E�@�{@��^@�X@���@���@�Q�@� �@��F@���@�=q@��^@�p�@��/@���@���@�Z@�(�@��@��@�l�@���@�+@�"�@���@�-@��@�x�@�G�@�%@��9@�1@��@�ƨ@��m@���@��@���@�l�@�K�@���@���@��\@��\@��\@�v�@�^5@�E�@�$�@���@���@��u@���@�C�@�33@�+@�"�@��!@�E�@�J@�@��h@�x�@�p�@�O�@�V@���@��9@�z�@�bN@� �@���@�dZ@�S�@�S�@�K�@�C�@�K�@�+@��H@���@�v�@�M�@�$�@�@���@���@���@��h@�hs@��@���@���@�r�@�bN@�(�@�  @�  @��;@��P@�l�@�33@�
=@��y@�ȴ@��!@�V@���@���@�7L@��@�V@���@���@��9@��@��u@�z�@�9X@�  @���@�t�@�+@��@��@�
=@���@�^5@�^5@�V@�$�@��@��T@��-@���@��@�hs@�O�@��@�V@���@��/@���@���@��w@�\)@�K�@�s@{��@i�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��A��yA��A��`A���A���A��hA�ffA�&�A���A��
A��hA�C�A��`A��A�x�A�v�A�r�A�n�A�jA�ffA�jA��DA��hA��7A��+A�z�A�K�A�7LA�VA��
A�ƨA�x�A�A��A��A��HA���A�r�A�\)A�XA�^5A�XA�M�A�G�A�G�A�E�A�?}A�(�A��A��A�oA�VA�JA�JA���A��A���A���A�K�A��`A�~�A�~�A�9XA�XA���A��jA��!A�M�A�~�A�VA���A�+A��A���A���A��!A���A�A��9A��uA��jA��#A�bNA�  A�{A�M�A�ƨA�Q�A��HA�;dA��uA��^A�O�A�A��DA�
=A��HA�~�A�1'A�?}A���A�-A�5?A��RA�ffA�M�A�^5A�K�A�9XA�"�A���A���A�^5A���A|�Ax�Aw33Av�yAudZAsdZAq/AoS�Al�HAk`BAj��AiG�Ae��Ac�FAb{A`v�A_�PA^v�A[XAY��AYS�AX�AW�;AU�7AS"�AR$�AQ�AQt�AQ&�AP�DAPM�AO�-ANVAH��AG�AGAF�AE��AE"�ADbNAC�TAC/AA��A@��A?�A??}A>n�A<�RA:�HA:1'A9/A7x�A6E�A6 �A5��A4��A2�HA2�A1��A0^5A/K�A.��A-��A,^5A*��A*�DA*bA)S�A'�;A%�A#��A"�A!33A A�A�A�\A�TA�A/A�\A�A9XA�AI�A�A��A�+A�mA�7A�AQ�A�A"�AE�A�7Ap�A��Az�A�A�FA;dAA�jA-A
��A
z�A
9XA	��A	G�Ar�A(�A�A�AM�A�A�\A7LA�A��A bN@��@���@���@�1@��`@���@��@��-@��@�j@��@�\@��D@�\)@�V@�G�@��/@�Ĝ@�@�@�@�j@�@���@��@�@�v�@�-@���@ߍP@��@��@�^5@��@�K�@�
=@�ȴ@�z�@�@�V@�bN@϶F@���@�Z@�|�@�p�@�r�@���@���@�S�@�J@š�@�%@å�@�@�G�@��`@���@��F@��@��^@�G�@���@�A�@���@���@��@�Ĝ@�b@���@�l�@�;d@���@��@���@�X@�/@���@�9X@��!@�E�@�=q@�5?@���@�?}@��D@��m@�dZ@�E�@�=q@��/@��@�Z@�A�@��@��P@�33@�@�^5@�@��#@��^@���@�p�@�%@��@�r�@��;@��w@��P@�;d@��+@�M�@�E�@�{@��^@�X@���@���@�Q�@� �@��F@���@�=q@��^@�p�@��/@���@���@�Z@�(�@��@��@�l�@���@�+@�"�@���@�-@��@�x�@�G�@�%@��9@�1@��@�ƨ@��m@���@��@���@�l�@�K�@���@���@��\@��\@��\@�v�@�^5@�E�@�$�@���@���@��u@���@�C�@�33@�+@�"�@��!@�E�@�J@�@��h@�x�@�p�@�O�@�V@���@��9@�z�@�bN@� �@���@�dZ@�S�@�S�@�K�@�C�@�K�@�+@��H@���@�v�@�M�@�$�@�@���@���@���@��h@�hs@��@���@���@�r�@�bN@�(�@�  @�  @��;@��P@�l�@�33@�
=@��y@�ȴ@��!@�V@���@���@�7L@��@�V@���@���@��9@��@��u@�z�@�9X@�  @���@�t�@�+@��@��@�
=@���@�^5@�^5@�V@�$�@��@��T@��-@���@��@�hs@�O�@��@�V@���@��/@���@���@��w@�\)@�K�@�s@{��@i�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B	7B	7B	7B	7B	7B	7B	7B	7B
=BJBJB\B�B)�B<jB?}BA�BA�BB�BA�BA�BB�BL�BS�BT�BT�BR�BO�BP�BW
BW
BXBW
BVBZBcTB�B�DB�1B�%B�+B�7B�DB�JB�JB�JB�JB�DB�1B�1B�1B�7B�=B�JB�JB�VB�\B�bB�oB��B��B��B��B��B��B��B��B��B�VB�7B�B~�B{�Bu�BiyBYBJ�B<jB(�B�BVBB��B�B�B�fB�BƨB�3B�B��B��B��B�Bz�Bn�BP�B;dB-B�BJB
��B
�`B
��B
ĜB
ŢB
��B
�/B
�HB
�HB
�NB
�/B
��B
�wB
��B
z�B
aHB
S�B
Q�B
D�B
5?B
�B
JB	��B	�yB	�5B	��B	�RB	�B	��B	��B	�oB	�=B	x�B	o�B	jB	dZB	]/B	O�B	D�B	@�B	?}B	?}B	=qB	9XB	9XB	7LB	.B	�B	!�B	!�B	�B	�B	�B	{B	hB	JB	B	  B��B��B��B�B�yB�`B�NB�BB�B�B�B�B��B��BɺBÖB�wB�jB�dB�XB�?B�-B�!B�B��B��B��B�hB�VB�JB�1B�+B�%B�%B�%B�B�B�B}�B{�Bz�Bx�Bv�Bt�Bs�Br�Bq�Bp�Bo�Bq�Bo�Bp�Bn�Bl�Bk�BjBiyBhsBhsBiyBiyBhsBhsBhsBhsBjBk�Bk�BjBl�Bp�Bt�Bv�Bv�Bx�By�Bw�Bu�Bs�Bq�Bp�Bo�Bm�Bm�Bl�Bk�Bk�BjBjBjBk�Bl�Bm�Bl�Bl�Bm�Bo�Br�Bu�Bw�Bx�B� B�B�B�B�+B�+B�+B�%B�+B�7B�=B�=B�oB��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�-B�dB�qB�}B��BĜBȴB��B��B��B��B��B�B�#B�HB�`B�yB�B�B��B��B��B��B��B��B	%B	PB	VB	VB	VB	\B	hB	uB	{B	{B	�B	�B	�B	 �B	"�B	#�B	(�B	,B	.B	-B	0!B	2-B	33B	49B	7LB	:^B	;dB	=qB	A�B	E�B	G�B	I�B	L�B	Q�B	S�B	S�B	VB	XB	\)B	^5B	bNB	dZB	e`B	hsB	m�B	o�B	o�B	o�B	p�B	s�B	s�B	s�B	t�B	u�B	w�B	{�B	� B	�B	�+B	�=B	�VB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�'B	�-B	�3B	�3B	�?B	�RB	�XB	�dB	�qB	�qB	�qB	�}B	��B	��B	B	ĜB	ĜB	ƨB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�B	�B	�B	�)B	�)B	�/B	�5B	�BB	�BB	�BB	�NB	�TB	�TB	�TB	�ZB	�`B	�fB	�fB	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
%B
%B
%B
%B
%B
B
B
B
�B
 vB
/ 222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  B	7B	7B	7B	7B	7B	7B	7B	7B
=BJBJB\B�B)�B<jB?}BA�BA�BB�BA�BA�BB�BL�BS�BT�BT�BR�BO�BP�BW
BW
BXBW
BVBZBcTB�B�DB�1B�%B�+B�7B�DB�JB�JB�JB�JB�DB�1B�1B�1B�7B�=B�JB�JB�VB�\B�bB�oB��B��B��B��B��B��B��B��B��B�VB�7B�B~�B{�Bu�BiyBYBJ�B<jB(�B�BVBB��B�B�B�fB�BƨB�3B�B��B��B��B�Bz�Bn�BP�B;dB-B�BJB
��B
�`B
��B
ĜB
ŢB
��B
�/B
�HB
�HB
�NB
�/B
��B
�wB
��B
z�B
aHB
S�B
Q�B
D�B
5?B
�B
JB	��B	�yB	�5B	��B	�RB	�B	��B	��B	�oB	�=B	x�B	o�B	jB	dZB	]/B	O�B	D�B	@�B	?}B	?}B	=qB	9XB	9XB	7LB	.B	�B	!�B	!�B	�B	�B	�B	{B	hB	JB	B	  B��B��B��B�B�yB�`B�NB�BB�B�B�B�B��B��BɺBÖB�wB�jB�dB�XB�?B�-B�!B�B��B��B��B�hB�VB�JB�1B�+B�%B�%B�%B�B�B�B}�B{�Bz�Bx�Bv�Bt�Bs�Br�Bq�Bp�Bo�Bq�Bo�Bp�Bn�Bl�Bk�BjBiyBhsBhsBiyBiyBhsBhsBhsBhsBjBk�Bk�BjBl�Bp�Bt�Bv�Bv�Bx�By�Bw�Bu�Bs�Bq�Bp�Bo�Bm�Bm�Bl�Bk�Bk�BjBjBjBk�Bl�Bm�Bl�Bl�Bm�Bo�Br�Bu�Bw�Bx�B� B�B�B�B�+B�+B�+B�%B�+B�7B�=B�=B�oB��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�-B�dB�qB�}B��BĜBȴB��B��B��B��B��B�B�#B�HB�`B�yB�B�B��B��B��B��B��B��B	%B	PB	VB	VB	VB	\B	hB	uB	{B	{B	�B	�B	�B	 �B	"�B	#�B	(�B	,B	.B	-B	0!B	2-B	33B	49B	7LB	:^B	;dB	=qB	A�B	E�B	G�B	I�B	L�B	Q�B	S�B	S�B	VB	XB	\)B	^5B	bNB	dZB	e`B	hsB	m�B	o�B	o�B	o�B	p�B	s�B	s�B	s�B	t�B	u�B	w�B	{�B	� B	�B	�+B	�=B	�VB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�'B	�-B	�3B	�3B	�?B	�RB	�XB	�dB	�qB	�qB	�qB	�}B	��B	��B	B	ĜB	ĜB	ƨB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�B	�B	�B	�)B	�)B	�/B	�5B	�BB	�BB	�BB	�NB	�TB	�TB	�TB	�ZB	�`B	�fB	�fB	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
%B
%B
%B
%B
%B
B
B
B
�B
 vB
/ 222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.33 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005191754                              AO  ARCAADJP                                                                    20181005191754    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005191754  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005191754  QCF$                G�O�G�O�G�O�8000            