CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:16:53Z creation      
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
_FillValue                    �0Argo profile    3.1 1.2 19500101000000  20181005191653  20181005191653  5904954 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               A   AO  6557                            2B  A   APEX                            7468                            062512                          846 @ׯ���y1   @ׯ�-��@4Nz�G��c��+1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      A   A   A   @@  @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B/��B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C�C  C  C�fC  C  C  C  C  C  C   C"  C$  C&  C'�fC*  C,  C-�fC0  C2  C4  C6�C8�C:  C<  C>  C?�fCB  CD  CF  CH  CJ  CL  CN  CP  CR  CT�CV�CX  CZ  C\  C^  C`  Cb�Cd  Cf�Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C��3C��3C�  C�  C��C��C�  C��3C�  C��3C��3C�  C�  C�  C�  C��3C��3C��3C��3C�  C��C�  C�  C��C�  C�  C�  C�  C��C��C��C��C��3C�  C��3C��3C��3C��3C��3C��C�  C��3C��fC�  C��C�  C��3C��3C��3C��3C�  C�  C��C�  C��C��C��C��C��C�  C�  C�  C��3C��3C�  C�  C�  C��3C��3C��3C��C�  C�  C�  C��C��3C��C��C��3C�  C�  C�  C��C�  C��3C�  C��C��C�  C��3C��C��3C�  C�  C��3C��C�  C��3C��C�  C�  C�  C�  C�  C�  C��C��3C�  C��C��C�  C�  C��C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C��C�  C�  D �D �fDfD� D  Ds3D�3D� DfD� D  D� D  D�fDfD� D  D� D��D	� D
fD
��DfD�fD  D� D  D�fD  Dy�D  D� D��Dy�D  D� DfD� D  D�fD  Dy�D  D� DfD� D  D� D��D�fD��D� D  D� D  Dy�D  D� D�3Dy�D�3Dy�D  D�fD fD y�D!  D!� D"  D"� D#  D#� D$fD$y�D%  D%� D&  D&�fD'�D'�fD(fD(�fD)  D)� D)��D*� D+fD+�fD,fD,�fD-fD-�fD.  D.� D/fD/�fD0fD0� D0��D1y�D2  D2� D3  D3� D4  D4y�D5  D5�fD6fD6�fD7fD7�fD8fD8�fD9�D9� D9��D:y�D:��D;y�D;��D<y�D=fD=� D>fD>� D?fD?�fD@  D@s3D@��DA�fDB  DBy�DB��DCy�DD  DD� DEfDE� DF  DF�fDG  DG� DH  DHy�DH��DI� DJ  DJ�fDK  DKy�DK��DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQy�DQ��DRs3DR�3DSy�DS��DTy�DT��DU� DV  DVy�DW  DW� DW��DX�fDYfDY� DY��DZy�D[  D[� D\  D\� D\��D]�fD^  D^�fD_  D_�fD`  D`y�D`�3Da� Db  Db� Dc  Dc� DdfDd�fDd��De� DffDf� DgfDg� Dh  Dhy�Dh��Di� DjfDj��Dk�Dk� Dk��Dl� Dm  Dmy�Dn  Dn�fDofDo� Do��Dpy�Dp��Dqy�Dr  Dr� Ds  Ds� Ds��Dt� DufDu� Dv  Dv�fDw  Dw�fDw��Dy�fD�%qD���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @H��@�z�@�z�A=qA"=qAB=qAb=qA��A��A��A��A��A��A��A��B �\B�\B�\B�\B �\B(�\B0(�B8�\B@�\BH�\BP�\BX�\B`�\Bh�\Bp�\Bx�\B�G�B�G�B�z�B�z�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�C #�C#�C#�C#�C#�C
#�C=qC#�C#�C
=C#�C#�C#�C#�C#�C#�C #�C"#�C$#�C&#�C(
=C*#�C,#�C.
=C0#�C2#�C4#�C6=qC8=qC:#�C<#�C>#�C@
=CB#�CD#�CF#�CH#�CJ#�CL#�CN#�CP#�CR#�CT=qCV=qCX#�CZ#�C\#�C^#�C`#�Cb=qCd#�Cf=qCh#�Cj#�Cl#�Cn#�Cp#�Cr#�Ct#�Cv#�Cx#�Cz#�C|#�C~#�C��C�C�C��C��C��C��C��C�C��C�C�C��C��C��C��C�C�C�C�C��C��C��C��C��C��C��C��C��C��C��C��C��C�C��C�C�C�C�C�C��C��C�C��RC��C��C��C�C�C�C�C��C��C��C��C��C��C��C��C��C��C��C��C�C�C��C��C��C�C�C�C��C��C��C��C��C�C��C��C�C��C��C��C��C��C�C��C��C��C��C�C��C�C��C��C�C��C��C�C��C��C��C��C��C��C��C��C�C��C��C��C��C��C��C��C��C��C�C��C��C��C��C��C��C��C��C��C��D �D �\D\D��D�D|)D�)D��D\D��D�D��D�D�\D\D��D�D��D	�D	��D
\D
��D\D�\D�D��D�D�\D�D��D�D��D�D��D�D��D\D��D�D�\D�D��D�D��D\D��D�D��D�D�\D�D��D�D��D�D��D�D��D�)D��D�)D��D�D�\D \D ��D!�D!��D"�D"��D#�D#��D$\D$��D%�D%��D&�D&�\D'�D'�\D(\D(�\D)�D)��D*�D*��D+\D+�\D,\D,�\D-\D-�\D.�D.��D/\D/�\D0\D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5�\D6\D6�\D7\D7�\D8\D8�\D9�D9��D:�D:��D;�D;��D<�D<��D=\D=��D>\D>��D?\D?�\D@�D@|)DA�DA�\DB�DB��DC�DC��DD�DD��DE\DE��DF�DF�\DG�DG��DH�DH��DI�DI��DJ�DJ�\DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR|)DR�)DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX�\DY\DY��DZ�DZ��D[�D[��D\�D\��D]�D]�\D^�D^�\D_�D_�\D`�D`��D`�)Da��Db�Db��Dc�Dc��Dd\Dd�\De�De��Df\Df��Dg\Dg��Dh�Dh��Di�Di��Dj\Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn�\Do\Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Du\Du��Dv�Dv�\Dw�Dw�\Dw��Dy�\D�)�D��H1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A���A���A�ȴA���Aӗ�AӃA�r�A�l�A�n�A�l�A�hsA�bNA�+A�A��A�oA��A�  A��yA�|�A�VA��mA���AѮAуA�x�A��mA��A�9XAΰ!A���A͗�Aͣ�Aͧ�A�~�A�ȴA�33A��/A��#A��;A��TA���A���A��A��A���A�jAȲ-A�p�A�S�A��A�A�`BA�9XAÝ�A���A���A���A���A���A��FA�(�A�VA��
A���A��A���A��A���A�v�A���A�XA�hsA��A�
=A��uA�
=A�ZA��A�l�A���A��A��9A�ƨA���A�^5A��A�v�A�&�A���A�Q�A���A���A� �A��A�`BA�"�A��A��TA�v�A���A�l�A�JA�K�A��DA�7LA��wA�G�A��A���A�~�A�ȴA�\)A�1A�n�A~�\A|-AyAvĜAsK�Aq��Am�AiC�Ad�jAc��Ab^5A^�!A[x�AY+AVZAS|�AQS�AN��AK�^AJbNAI%AG
=AD�ACVA@��A>-A=&�A<��A;��A9�A6jA3��A2~�A2 �A1�A0I�A/&�A.��A.�uA.�\A.ffA-�TA+��A*E�A)��A(ȴA&�+A$bNA#��A"��A�A��AȴA��AbA�A  A^5A��A��A�#A�wA�PAx�A�^AQ�AVAJA�RA-AQ�A9XA�A�PAoA�`A�\AffAA/Ar�A�A��An�A�7Ap�AbNA��A5?A��AhsA ��@���@���@���@���@���@��!@��F@��@�@�x�@�  @�"�@��-@�n�@�33@���@�M�@���@�V@���@��@�F@��@��`@���@�~�@���@�hs@�G�@�9X@�p�@�!@߶F@��y@���@�  @�ff@�9X@�=q@�1@�l�@���@·+@�@�O�@��@̃@�b@��m@��m@˶F@�t�@��@Ɂ@�t�@��@��@�7L@ÍP@���@��@��j@���@�dZ@��@���@���@�^5@��#@�/@�%@��@��u@�j@�Q�@�b@���@��@�5?@��7@���@���@��@�j@�Q�@� �@��@���@�;d@��y@��+@��\@�$�@���@��@�x�@���@�9X@��@���@�S�@�o@��H@��R@��+@�~�@�V@��#@�/@���@���@�Q�@��@�|�@�;d@�+@�o@��y@��@���@���@��+@�^5@�ff@�^5@�=q@��@�{@��@��@��^@�X@���@��m@�33@�"�@�K�@��R@���@�Ĝ@�(�@���@�K�@�ȴ@��+@���@���@��y@��@���@�V@��^@�bN@��m@��F@��;@�1'@��@��@�o@�@��@�ff@�-@�=q@��@��7@�hs@�`B@�G�@��@�1@�|�@���@���@���@��!@���@�^5@�-@�@��@�@���@��h@��h@�p�@��@���@���@�I�@� �@��@�b@�1@�  @��@��m@���@��w@��@���@���@���@���@���@���@���@���@�l�@�~�@�v�@�ff@��@���@�x�@�hs@�G�@�G�@�G�@�7L@���@�Ĝ@���@�j@��@��F@���@�|�@�S�@�o@��@��R@�v�@�E�@�5?@��@�J@��@���@���@��@���@��@��@��F@��@�|�@�ȴ@�5?@��#@�p�@�/@��@�Ĝ@��@�ƨ@��@��@�33@��!@�E�@�$�@���@���@�r�@��@��;@�
=@���@��!@��\@��+@�E�@��h@�Ĝ@�1'@���@�dZ@�"�@�o@�@�~�@��@���@���@��h@��7@�p�@��@z�@m*01111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A���A���A�ȴA���Aӗ�AӃA�r�A�l�A�n�A�l�A�hsA�bNA�+A�A��A�oA��A�  A��yA�|�A�VA��mA���AѮAуA�x�A��mA��A�9XAΰ!A���A͗�Aͣ�Aͧ�A�~�A�ȴA�33A��/A��#A��;A��TA���A���A��A��A���A�jAȲ-A�p�A�S�A��A�A�`BA�9XAÝ�A���A���A���A���A���A��FA�(�A�VA��
A���A��A���A��A���A�v�A���A�XA�hsA��A�
=A��uA�
=A�ZA��A�l�A���A��A��9A�ƨA���A�^5A��A�v�A�&�A���A�Q�A���A���A� �A��A�`BA�"�A��A��TA�v�A���A�l�A�JA�K�A��DA�7LA��wA�G�A��A���A�~�A�ȴA�\)A�1A�n�A~�\A|-AyAvĜAsK�Aq��Am�AiC�Ad�jAc��Ab^5A^�!A[x�AY+AVZAS|�AQS�AN��AK�^AJbNAI%AG
=AD�ACVA@��A>-A=&�A<��A;��A9�A6jA3��A2~�A2 �A1�A0I�A/&�A.��A.�uA.�\A.ffA-�TA+��A*E�A)��A(ȴA&�+A$bNA#��A"��A�A��AȴA��AbA�A  A^5A��A��A�#A�wA�PAx�A�^AQ�AVAJA�RA-AQ�A9XA�A�PAoA�`A�\AffAA/Ar�A�A��An�A�7Ap�AbNA��A5?A��AhsA ��@���@���@���@���@���@��!@��F@��@�@�x�@�  @�"�@��-@�n�@�33@���@�M�@���@�V@���@��@�F@��@��`@���@�~�@���@�hs@�G�@�9X@�p�@�!@߶F@��y@���@�  @�ff@�9X@�=q@�1@�l�@���@·+@�@�O�@��@̃@�b@��m@��m@˶F@�t�@��@Ɂ@�t�@��@��@�7L@ÍP@���@��@��j@���@�dZ@��@���@���@�^5@��#@�/@�%@��@��u@�j@�Q�@�b@���@��@�5?@��7@���@���@��@�j@�Q�@� �@��@���@�;d@��y@��+@��\@�$�@���@��@�x�@���@�9X@��@���@�S�@�o@��H@��R@��+@�~�@�V@��#@�/@���@���@�Q�@��@�|�@�;d@�+@�o@��y@��@���@���@��+@�^5@�ff@�^5@�=q@��@�{@��@��@��^@�X@���@��m@�33@�"�@�K�@��R@���@�Ĝ@�(�@���@�K�@�ȴ@��+@���@���@��y@��@���@�V@��^@�bN@��m@��F@��;@�1'@��@��@�o@�@��@�ff@�-@�=q@��@��7@�hs@�`B@�G�@��@�1@�|�@���@���@���@��!@���@�^5@�-@�@��@�@���@��h@��h@�p�@��@���@���@�I�@� �@��@�b@�1@�  @��@��m@���@��w@��@���@���@���@���@���@���@���@���@�l�@�~�@�v�@�ff@��@���@�x�@�hs@�G�@�G�@�G�@�7L@���@�Ĝ@���@�j@��@��F@���@�|�@�S�@�o@��@��R@�v�@�E�@�5?@��@�J@��@���@���@��@���@��@��@��F@��@�|�@�ȴ@�5?@��#@�p�@�/@��@�Ĝ@��@�ƨ@��@��@�33@��!@�E�@�$�@���@���@�r�@��@��;@�
=@���@��!@��\@��+@�E�@��h@�Ĝ@�1'@���@�dZ@�"�@�o@�@�~�@��@���@���@��h@��7@�p�@��@z�@m*01111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
�+B
�+B
�+B
�+B
�+B
�+B
�1B
�=B
�=B
�=B
�JB
�JB
�bB
��B
��B
�dB
��B
�/B
�BB
�/B
�;B
�B
��BBBB
��B
�/B
�fB
�ZB
�B
�B
��B
��B  B
��B �B/B0!B1'B1'B9XBR�Bq�Bw�B{�B�=B��B��B�B�PB�'B��B�TB	7B$�BN�Bq�BhsBw�B��B�wB�ZB�B�B�B�B�B�B�yB�sB�sB�TB�B��B��B��BŢB��B�dB�!B��B��B�PBs�BcTBZBO�B<jB%�BbB��B�B�#B�wB�BbNB\)BYBM�B0!BB
�B
��B
�!B
��B
��B
�DB
_;B
D�B
7LB
/B
(�B
"�B
�B
B	�B	�TB	��B	�}B	�3B	��B	z�B	dZB	]/B	T�B	B�B	49B	(�B	"�B	�B	JB	B��B��B�B�B�ZB�5B�B�
B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B��B��BȴBB��B�}B�wB�qB�^B�RB�XB�dBÖBǮBǮBƨBĜB�}B�RB�'B�B��B��B�B�3B�3B�B��B��B�B�FB��BɺB�B��BÖB�qB�XB�LB�-B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��BǮB�B�HB�`B	  B	%B	B��B��B�B�B�B�B��B��B��B��B��B��BɺB��B��B��B��BƨBÖBÖBɺB��B��B��B��B��B�B�B�;B�HB�NB�TB�ZB�`B�sB�B�B�B�B�B�B�B�B��B��B��B��B	  B	B	%B	DB	PB	\B	oB	uB	{B	�B	�B	�B	�B	#�B	%�B	&�B	'�B	(�B	)�B	+B	,B	-B	-B	.B	33B	9XB	>wB	?}B	B�B	B�B	C�B	B�B	C�B	D�B	E�B	F�B	G�B	I�B	J�B	J�B	J�B	M�B	Q�B	S�B	S�B	XB	ZB	^5B	aHB	dZB	hsB	iyB	iyB	k�B	k�B	l�B	o�B	p�B	p�B	q�B	s�B	v�B	v�B	x�B	x�B	{�B	�B	�B	�+B	�1B	�DB	�DB	�=B	�7B	�7B	�7B	�DB	�JB	�PB	�VB	�\B	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�!B	�!B	�'B	�'B	�-B	�3B	�3B	�9B	�9B	�?B	�FB	�FB	�FB	�FB	�XB	�XB	�^B	�wB	�}B	��B	��B	��B	��B	��B	ÖB	ĜB	ŢB	ƨB	ƨB	ƨB	ƨB	ƨB	ƨB	ƨB	ƨB	ƨB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�B	�B	�B	�B	�#B	�)B	�/B	�/B	�5B	�5B	�;B	�BB	�HB	�HB	�NB	�TB	�mB	�mB	�mB	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
B
B
B
B
B
B
+B
�B

B
'R2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 B
�+B
�+B
�+B
�+B
�+B
�+B
�1B
�=B
�=B
�=B
�JB
�JB
�bB
��B
��B
�dB
��B
�/B
�BB
�/B
�;B
�B
��BBBB
��B
�/B
�fB
�ZB
�B
�B
��B
��B  B
��B �B/B0!B1'B1'B9XBR�Bq�Bw�B{�B�=B��B��B�B�PB�'B��B�TB	7B$�BN�Bq�BhsBw�B��B�wB�ZB�B�B�B�B�B�B�yB�sB�sB�TB�B��B��B��BŢB��B�dB�!B��B��B�PBs�BcTBZBO�B<jB%�BbB��B�B�#B�wB�BbNB\)BYBM�B0!BB
�B
��B
�!B
��B
��B
�DB
_;B
D�B
7LB
/B
(�B
"�B
�B
B	�B	�TB	��B	�}B	�3B	��B	z�B	dZB	]/B	T�B	B�B	49B	(�B	"�B	�B	JB	B��B��B�B�B�ZB�5B�B�
B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B��B��BȴBB��B�}B�wB�qB�^B�RB�XB�dBÖBǮBǮBƨBĜB�}B�RB�'B�B��B��B�B�3B�3B�B��B��B�B�FB��BɺB�B��BÖB�qB�XB�LB�-B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��BǮB�B�HB�`B	  B	%B	B��B��B�B�B�B�B��B��B��B��B��B��BɺB��B��B��B��BƨBÖBÖBɺB��B��B��B��B��B�B�B�;B�HB�NB�TB�ZB�`B�sB�B�B�B�B�B�B�B�B��B��B��B��B	  B	B	%B	DB	PB	\B	oB	uB	{B	�B	�B	�B	�B	#�B	%�B	&�B	'�B	(�B	)�B	+B	,B	-B	-B	.B	33B	9XB	>wB	?}B	B�B	B�B	C�B	B�B	C�B	D�B	E�B	F�B	G�B	I�B	J�B	J�B	J�B	M�B	Q�B	S�B	S�B	XB	ZB	^5B	aHB	dZB	hsB	iyB	iyB	k�B	k�B	l�B	o�B	p�B	p�B	q�B	s�B	v�B	v�B	x�B	x�B	{�B	�B	�B	�+B	�1B	�DB	�DB	�=B	�7B	�7B	�7B	�DB	�JB	�PB	�VB	�\B	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�!B	�!B	�'B	�'B	�-B	�3B	�3B	�9B	�9B	�?B	�FB	�FB	�FB	�FB	�XB	�XB	�^B	�wB	�}B	��B	��B	��B	��B	��B	ÖB	ĜB	ŢB	ƨB	ƨB	ƨB	ƨB	ƨB	ƨB	ƨB	ƨB	ƨB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�B	�B	�B	�B	�#B	�)B	�/B	�/B	�5B	�5B	�;B	�BB	�HB	�HB	�NB	�TB	�mB	�mB	�mB	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
B
B
B
B
B
B
+B
�B

B
'R2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.14 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005191653                              AO  ARCAADJP                                                                    20181005191653    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005191653  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005191653  QCF$                G�O�G�O�G�O�8000            