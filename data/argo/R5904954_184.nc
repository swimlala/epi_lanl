CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:17:31Z creation      
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
resolution        =���   axis      Z        p  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  @�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     p  B�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J,   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     p  L   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     p  Sx   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  Z�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     p  \�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  d4   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     p  f   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     p  m�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  t�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     p  v�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ~<   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     p  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �4   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �D   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �H   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �X   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �\   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �`   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �dArgo profile    3.1 1.2 19500101000000  20181005191731  20181005191731  5904954 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6557                            2B  A   APEX                            7468                            062512                          846 @��d�k�1   @��es���@6cn��P�d��Q�1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �A   A   A   @@  @�  @�  A   A   A@  A`  A�  A�  A�  A�33A�  A�  A�  A�  B   B  B  B  B   B(ffB0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bw��B��B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�33B�  B�33B�  B�  B�33B�  B�  B���C  C  C  C  C
�C�C  C  C  C  C  C  C�fC  C  C   C!�fC#�fC&  C(  C*�C,�C.�C0�C2�C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP�CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cc�fCf  Ch  Cj  Cl  Cn�Cp  Cr  Cs�fCv  Cx  Cz  C|  C}�fC�  C��3C��3C�  C�  C��C��C�  C�  C�  C�  C�  C��3C�  C�  C�  C��C�  C��3C��3C�  C�  C�  C�  C��C�  C��C��C�  C�  C�  C��C�  C��3C��C��C�  C�  C��C�  C��C�  C��3C��C��C��3C�  C��3C��3C�  C�  C�  C�  C��C�  C��3C�  C�  C��3C�  C��3C�  C�  C�  C�  C��C�  C��C��3C�  C��C�  C�  C�  C�  C�  C��3C��3C�  C��C��C�  C�  C�  C��3C��3C��3C��fC��3C�  C�  C��3C��3C��3C��C��C�  C��3C�  C�  C��fC��3C��3C��3C��3C��3D y�D  D�fDfD�fD  Dy�D�3Dy�D��Dy�D��Dy�D  D� DfD�fD	  D	y�D	��D
�fD  D� D�D� D  D� D  D� DfD�fD  Dy�D��Dy�D��D� D  D� D��Dy�D��D� DfD�fDfDy�D��D� D��D� DfD� D  D�fDfD�fD  D�fDfD� D  D� D   D �fD!fD!� D"  D"�fD#fD#� D#��D$y�D%  D%�fD&  D&� D'  D'y�D(  D(�fD)fD)� D*  D*� D+fD+� D+��D,� D-fD-� D.  D.y�D/  D/y�D0  D0�fD1  D1y�D2  D2�fD3fD3�fD4  D4� D5  D5y�D6  D6�fD7  D7�fD8  D8� D9  D9� D:  D:�fD;fD;�fD<  D<y�D<��D=�fD>fD>y�D?  D?�fD@fD@� D@�3DAs3DB  DB� DC  DC� DDfDD� DEfDE� DE��DF�fDG  DG� DH  DH�fDIfDI� DJ  DJ� DJ��DK� DLfDLy�DL�3DMy�DM��DNy�DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS�fDTfDT� DU  DU�fDVfDV�fDW  DW�fDX  DXy�DX��DY� DZ  DZ� D[  D[� D\  D\�fD]  D]� D^  D^y�D^��D_y�D`  D`y�D`��Da� Da��Db� DcfDc� Dd  Dd�fDefDe�fDffDf�fDgfDg�fDhfDh�fDifDi� DjfDj� Dk  Dk� DlfDl��Dm�Dm�fDn  Dn� Do  Do�fDpfDp� Dq  Dq� Dr  Dr� Ds  Ds�fDs��Dty�Du  Du�fDvfDv�fDv��Dw� Dw� Dy�HD�?\D��f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@B�\@�G�@�G�A ��A ��A@��A`��A�Q�A�Q�A�Q�A��A�Q�A�Q�A�Q�A�Q�B (�B(�B(�B(�B (�B(�\B0(�B8(�B@(�BH(�BP(�BX(�B`(�Bh(�Bp(�BwBB��HB�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B��HB�{B�{B�{B�{B�{B�{B�G�B�{B�G�B�{B�{B�G�B�{B�{B��HC
=C
=C
=C
=C
#�C#�C
=C
=C
=C
=C
=C
=C�C
=C
=C 
=C!�C#�C&
=C(
=C*#�C,#�C.#�C0#�C2#�C4
=C6
=C8
=C:
=C<
=C>
=C@
=CB
=CD
=CF
=CH
=CJ
=CL
=CN
=CP#�CR
=CT
=CV
=CX
=CZ
=C\
=C^
=C`
=Cb
=Cc�Cf
=Ch
=Cj
=Cl
=Cn#�Cp
=Cr
=Cs�Cv
=Cx
=Cz
=C|
=C}�C�C��RC��RC�C�C��C��C�C�C�C�C�C��RC�C�C�C��C�C��RC��RC�C�C�C�C��C�C��C��C�C�C�C��C�C��RC��C��C�C�C��C�C��C�C��RC��C��C��RC�C��RC��RC�C�C�C�C��C�C��RC�C�C��RC�C��RC�C�C�C�C��C�C��C��RC�C��C�C�C�C�C�C��RC��RC�C��C��C�C�C�C��RC��RC��RC��C��RC�C�C��RC��RC��RC��C��C�C��RC�C�C��C��RC��RC��RC��RC��RD |)D�D��D�D��D�D|)D��D|)D�)D|)D�)D|)D�D��D�D��D	�D	|)D	�)D
��D�D��D\D��D�D��D�D��D�D��D�D|)D�)D|)D�)D��D�D��D�)D|)D�)D��D�D��D�D|)D�)D��D�)D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D#�)D$|)D%�D%��D&�D&��D'�D'|)D(�D(��D)�D)��D*�D*��D+�D+��D+�)D,��D-�D-��D.�D.|)D/�D/|)D0�D0��D1�D1|)D2�D2��D3�D3��D4�D4��D5�D5|)D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<|)D<�)D=��D>�D>|)D?�D?��D@�D@��D@��DAu�DB�DB��DC�DC��DD�DD��DE�DE��DE�)DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DJ�)DK��DL�DL|)DL��DM|)DM�)DN|)DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX|)DX�)DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^|)D^�)D_|)D`�D`|)D`�)Da��Da�)Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl�\Dm\Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Ds�)Dt|)Du�Du��Dv�Dv��Dv�)Dw��Dw�Dy��D�@�D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Aΰ!Aβ-Aδ9Aδ9AζFAζFAθRAθRAκ^Aκ^AμjAμjAξwAξwAξwA���A���A���A�A�A�A���A�A�A�A�ĜA�ĜA�ƨA�ƨA�ĜA�ƨA�ƨA�ȴA���A���A���A���A���A���A���A���A���A���Aδ9Aΰ!AάAΙ�A�~�A�dZA�5?A�/A�-A�$�A��A�{A�1A��A���AͮA͡�A�oA��#A�JA�-A�?}A���A���A��HA���A�M�A�n�A��-A���A��-A��#A���A���A��A���A�hsA�A�A�;dA���A�$�A��wA�(�A�bNA���A��-A���A��A�=qA���A��DA�E�A�JA��!A���A�$�A� �A�p�A�\)A��A��`A� �A�hsA��A��
A��A�VA�VA�I�A���A��A�
=A���A��\A���A�M�A���A�n�A��A���A��A�E�A�{A�=qAt�A|1'Az��Az  AyƨAyXAx��AxffAxAw�Au�AsXAp�!Ao+AmK�Al{Ak;dAF�jAEƨAES�AD��ABr�A@Q�A>{A<$�A;��A:M�A933A8�A7&�A5�A3�mA3&�A2Q�A21'A1��A0��A01A-�A+��A*^5A)�;A)�A)p�A(�`A'�A&��A%33A#�A!�hA �A A�A%AjAhsAbNA��AhsA�A��An�A^5A1'A�A�`A��A��A��A�A�
A�yA5?AQ�A��A�A�`A��A
��A
A	�A�DA��AG�A&�A�AVA%A��A��A�/A��A�A��Av�A��A��AG�A�HAo@�x�@�Q�@�  @��@�?}@��-@���@�?}@��@�%@���@�  @��R@�ff@�?}@�ff@�Q�@�M�@�v�@�M�@�I�@�^5@�u@��@�z�@�
=@�+@�C�@ܼj@ڸR@� �@ָR@�p�@�bN@ӥ�@�`B@�bN@�K�@�=q@ͩ�@�1@�+@�
=@��H@�-@�z�@�9X@ț�@�Z@��m@Ƈ+@�7L@��/@��
@�
=@�E�@��#@�r�@���@��@�n�@���@���@��
@���@�@��@���@���@�O�@�%@���@���@�t�@�+@��7@��D@�Z@��m@�;d@�ȴ@�ff@��@���@�/@�I�@�  @��
@��P@�|�@��@�;d@���@���@�V@�^5@�5?@�^5@�v�@�~�@��\@�~�@�J@�x�@���@��`@�&�@���@��-@�/@���@� �@���@���@�o@�~�@���@�J@�$�@�=q@�M�@��@�{@�O�@�bN@��m@�dZ@��@�v�@�@���@�O�@��u@�9X@�1@�ƨ@�t�@�dZ@�o@���@�=q@�^5@���@���@���@��D@�bN@�ƨ@���@�dZ@���@��!@��+@���@�;d@���@�^5@��@��-@�`B@�/@��@��`@��/@��9@�b@��@�V@�x�@��-@� �@���@���@���@�|�@�S�@�S�@�33@��@��H@��H@�@�+@���@� �@���@���@�dZ@�o@�
=@���@�C�@�K�@��!@�ff@���@�7L@�7L@��j@��@���@�O�@���@��@�G�@�&�@�/@�V@��/@���@���@�&�@��@�%@��/@�Ĝ@��u@�ƨ@�"�@��!@��T@�O�@��^@��@���@�z�@�1'@��@��;@�|�@�
=@��!@��+@�~�@���@��\@�v�@�^5@�-@�J@��#@���@���@�x�@�hs@�X@�/@�Y@~��@m;11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111Aΰ!Aβ-Aδ9Aδ9AζFAζFAθRAθRAκ^Aκ^AμjAμjAξwAξwAξwA���A���A���A�A�A�A���A�A�A�A�ĜA�ĜA�ƨA�ƨA�ĜA�ƨA�ƨA�ȴA���A���A���A���A���A���A���A���A���A���Aδ9Aΰ!AάAΙ�A�~�A�dZA�5?A�/A�-A�$�A��A�{A�1A��A���AͮA͡�A�oA��#A�JA�-A�?}A���A���A��HA���A�M�A�n�A��-A���A��-A��#A���A���A��A���A�hsA�A�A�;dA���A�$�A��wA�(�A�bNA���A��-A���A��A�=qA���A��DA�E�A�JA��!A���A�$�A� �A�p�A�\)A��A��`A� �A�hsA��A��
A��A�VA�VA�I�A���A��A�
=A���A��\A���A�M�A���A�n�A��A���A��A�E�A�{A�=qAt�A|1'Az��Az  AyƨAyXAx��AxffAxAw�Au�AsXAp�!Ao+AmK�Al{Ak;dAF�jAEƨAES�AD��ABr�A@Q�A>{A<$�A;��A:M�A933A8�A7&�A5�A3�mA3&�A2Q�A21'A1��A0��A01A-�A+��A*^5A)�;A)�A)p�A(�`A'�A&��A%33A#�A!�hA �A A�A%AjAhsAbNA��AhsA�A��An�A^5A1'A�A�`A��A��A��A�A�
A�yA5?AQ�A��A�A�`A��A
��A
A	�A�DA��AG�A&�A�AVA%A��A��A�/A��A�A��Av�A��A��AG�A�HAo@�x�@�Q�@�  @��@�?}@��-@���@�?}@��@�%@���@�  @��R@�ff@�?}@�ff@�Q�@�M�@�v�@�M�@�I�@�^5@�u@��@�z�@�
=@�+@�C�@ܼj@ڸR@� �@ָR@�p�@�bN@ӥ�@�`B@�bN@�K�@�=q@ͩ�@�1@�+@�
=@��H@�-@�z�@�9X@ț�@�Z@��m@Ƈ+@�7L@��/@��
@�
=@�E�@��#@�r�@���@��@�n�@���@���@��
@���@�@��@���@���@�O�@�%@���@���@�t�@�+@��7@��D@�Z@��m@�;d@�ȴ@�ff@��@���@�/@�I�@�  @��
@��P@�|�@��@�;d@���@���@�V@�^5@�5?@�^5@�v�@�~�@��\@�~�@�J@�x�@���@��`@�&�@���@��-@�/@���@� �@���@���@�o@�~�@���@�J@�$�@�=q@�M�@��@�{@�O�@�bN@��m@�dZ@��@�v�@�@���@�O�@��u@�9X@�1@�ƨ@�t�@�dZ@�o@���@�=q@�^5@���@���@���@��D@�bN@�ƨ@���@�dZ@���@��!@��+@���@�;d@���@�^5@��@��-@�`B@�/@��@��`@��/@��9@�b@��@�V@�x�@��-@� �@���@���@���@�|�@�S�@�S�@�33@��@��H@��H@�@�+@���@� �@���@���@�dZ@�o@�
=@���@�C�@�K�@��!@�ff@���@�7L@�7L@��j@��@���@�O�@���@��@�G�@�&�@�/@�V@��/@���@���@�&�@��@�%@��/@�Ĝ@��u@�ƨ@�"�@��!@��T@�O�@��^@��@���@�z�@�1'@��@��;@�|�@�
=@��!@��+@�~�@���@��\@�v�@�^5@�-@�J@��#@���@���@�x�@�hs@�X@�/@�Y@~��@m;11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�sB�sB�sB�sB�sB�mB�sB�mB�sB�mB�sB�sB�sB�sB�sB�sB�sB�sB�sB�sB�sB�sB�sB�sB�sB�sB�sB�sB�sB�sB�sB�sB�sB�sB�sB�sB�yB�sB�yB�sB�sB�yB�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B  B  B��B��B��BB!�B5?B0!B@�BaHBv�B�VB�PB��B��B�LB�qBƨB��B��B��BȴB��B�RB�9B�-B��B��B�uBz�Bn�BffBaHB[#BL�B<jB%�B�BbB  B��B��B�B�B��BÖB�XB�B��B�BhsB[#BL�B>wB%�B�B{BoBDBB
��B
��B
�B
�fB
��B
��B
�B
��B
�7B
q�B
ffB
_;B
]/B
YB
VB
R�B
N�B
F�B
<jB
,B
�B
VB
B	��B	�B	$�B	�B	�B	�B	PB��B�B�B�sB�`B�HB�;B�#B�B��B��B��B��B��BǮBB�^B�FB�3B�'B�B�B�B��B��B��B��B�{B�hB�bB�PB�=B�7B�%B�B�B�B�B�B� B~�B|�Bz�Bw�Bt�Br�Bp�Bl�BiyBffBbNB]/B[#BXBVBT�BS�BR�BQ�BS�BS�BS�BS�BS�BS�BS�BS�BS�BS�B[#B^5B`BBbNBaHB_;B[#BP�BD�BC�BE�BP�B]/BdZBffBhsBhsBjBm�Bm�BiyBgmBaHBS�BG�BK�BM�BP�BQ�BS�BT�BR�BO�BO�BR�BVBS�BQ�BYB[#B\)B\)B[#B^5B_;BaHBcTBdZBe`BffBgmBo�Bo�BjBm�Br�Bz�B}�B~�B~�B}�B}�B|�B~�B� B|�Bw�Bt�Br�Bq�Bp�Bn�Bn�Bn�Bn�Bp�Bq�Bs�Bt�Bu�Bx�Bx�By�B~�B�B�B�B�%B�DB�\B�{B��B��B��B��B�B�'B�FB�LB�XB�XB�jB��B��B�B�)B�;B�BB�HB�fB�B�B��B��B��B	B	
=B	hB	hB	�B	�B	�B	�B	"�B	'�B	)�B	+B	-B	0!B	2-B	9XB	8RB	6FB	5?B	5?B	6FB	8RB	:^B	:^B	<jB	@�B	B�B	C�B	F�B	G�B	I�B	L�B	Q�B	VB	YB	\)B	^5B	_;B	aHB	aHB	ffB	jB	k�B	k�B	m�B	p�B	t�B	~�B	�B	�B	�B	�B	�1B	�VB	�bB	�hB	�oB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�-B	�RB	�qB	�qB	�jB	�dB	�^B	�qB	�wB	��B	��B	��B	�}B	�wB	��B	ÖB	B	ÖB	ĜB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�#B	�)B	�)B	�)B	�#B	�B	�B	�B	�
B	�)B	�NB	�NB	�ZB	�ZB	�ZB	�ZB	�ZB	�ZB	�`B	�mB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�TB

#B
/22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222B�sB�sB�sB�sB�sB�mB�sB�mB�sB�mB�sB�sB�sB�sB�sB�sB�sB�sB�sB�sB�sB�sB�sB�sB�sB�sB�sB�sB�sB�sB�sB�sB�sB�sB�sB�sB�yB�sB�yB�sB�sB�yB�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B  B  B��B��B��BB!�B5?B0!B@�BaHBv�B�VB�PB��B��B�LB�qBƨB��B��B��BȴB��B�RB�9B�-B��B��B�uBz�Bn�BffBaHB[#BL�B<jB%�B�BbB  B��B��B�B�B��BÖB�XB�B��B�BhsB[#BL�B>wB%�B�B{BoBDBB
��B
��B
�B
�fB
��B
��B
�B
��B
�7B
q�B
ffB
_;B
]/B
YB
VB
R�B
N�B
F�B
<jB
,B
�B
VB
B	��B	�B	$�B	�B	�B	�B	PB��B�B�B�sB�`B�HB�;B�#B�B��B��B��B��B��BǮBB�^B�FB�3B�'B�B�B�B��B��B��B��B�{B�hB�bB�PB�=B�7B�%B�B�B�B�B�B� B~�B|�Bz�Bw�Bt�Br�Bp�Bl�BiyBffBbNB]/B[#BXBVBT�BS�BR�BQ�BS�BS�BS�BS�BS�BS�BS�BS�BS�BS�B[#B^5B`BBbNBaHB_;B[#BP�BD�BC�BE�BP�B]/BdZBffBhsBhsBjBm�Bm�BiyBgmBaHBS�BG�BK�BM�BP�BQ�BS�BT�BR�BO�BO�BR�BVBS�BQ�BYB[#B\)B\)B[#B^5B_;BaHBcTBdZBe`BffBgmBo�Bo�BjBm�Br�Bz�B}�B~�B~�B}�B}�B|�B~�B� B|�Bw�Bt�Br�Bq�Bp�Bn�Bn�Bn�Bn�Bp�Bq�Bs�Bt�Bu�Bx�Bx�By�B~�B�B�B�B�%B�DB�\B�{B��B��B��B��B�B�'B�FB�LB�XB�XB�jB��B��B�B�)B�;B�BB�HB�fB�B�B��B��B��B	B	
=B	hB	hB	�B	�B	�B	�B	"�B	'�B	)�B	+B	-B	0!B	2-B	9XB	8RB	6FB	5?B	5?B	6FB	8RB	:^B	:^B	<jB	@�B	B�B	C�B	F�B	G�B	I�B	L�B	Q�B	VB	YB	\)B	^5B	_;B	aHB	aHB	ffB	jB	k�B	k�B	m�B	p�B	t�B	~�B	�B	�B	�B	�B	�1B	�VB	�bB	�hB	�oB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�-B	�RB	�qB	�qB	�jB	�dB	�^B	�qB	�wB	��B	��B	��B	�}B	�wB	��B	ÖB	B	ÖB	ĜB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�#B	�)B	�)B	�)B	�#B	�B	�B	�B	�
B	�)B	�NB	�NB	�ZB	�ZB	�ZB	�ZB	�ZB	�ZB	�`B	�mB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�TB

#B
/22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.04 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005191731                              AO  ARCAADJP                                                                    20181005191731    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005191731  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005191731  QCF$                G�O�G�O�G�O�8000            