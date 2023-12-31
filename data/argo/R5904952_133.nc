CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:05:34Z creation      
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
_FillValue                    �dArgo profile    3.1 1.2 19500101000000  20181005190534  20181005190534  5904952 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6431                            2B  A   APEX                            7466                            062512                          846 @�ک�H#�1   @�ڪb��@0��1'�c�V�u1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �A   A   A   @��@�  @�  A   A   A@  A`  A�  A�33A�  A�  A�  A�  A�  A�  B   B��B  BffB   B(  B0  B8  B@  BH  BP  BXffB`ffBh  Bp  Bx  B��B���B���B�  B�  B�  B�  B�33B�33B�  B���B���B���B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  C   C��C�fC�fC  C
  C  C  C  C  C  C  C  C�fC  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CW�fCZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx�Cz�C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C��C�  C�  C�  C�  C��C�  C��3C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C��C��C�  C�  C��3C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C��C�  C�  C�  C��3C��fC�  C�  C�  C��C��D fD � D ��Dy�D��D� D  D� DfD� D  Dy�D  D� D  D� DfD� D	  D	�fD
fD
�fD  D� D  D� DfD� D  D�fD  Dy�D  D� D  D� D  D� D  D�fD  D� D  D�fD  D� D  Dy�D��Dy�D��Dy�D��Dy�D  D� DfD�fD  D� D  D� D��D� D   D � D!  D!y�D"  D"�fD#fD#� D$  D$� D%  D%� D&  D&�fD'  D'� D(  D(� D)  D)�fD*fD*� D+  D+�fD,  D,� D-  D-�fD.  D.y�D/  D/�fD0  D0� D1  D1� D2  D2� D3fD3�fD4fD4� D5  D5� D6fD6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� DHy�DH��DI� DJ  DJ� DK  DKy�DK��DLy�DM  DM� DM��DNy�DO  DO�fDP  DP� DP��DQy�DR  DR� DR��DSy�DT  DT� DU  DU�fDVfDV�fDW  DWy�DW��DXy�DY  DY� DZ  DZ� DZ��D[� D\  D\� D]  D]y�D^  D^�fD_fD_� D_��D`� DafDa� Db  Db�fDc  Dc� Dd  Dd�fDe  De� Df  Df� Dg  Dg� Dh  Dh� Dh��Diy�Dj  Dj� Dk  Dky�Dl  Dl� Dm  Dm� Dn  Dn�fDo  Do� Dp  Dp� Dq  Dq� Dr  Dr� Dr��Ds� Dt  Dt� Du  Du� DvfDv�fDwfDw� Dw��Dyq�D�<�D��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@+�@���@���Az�A$z�ADz�Adz�A�=qA�p�A�=qA�=qA�=qA�=qA�=qA�=qB�B�RB�B�B!�B)�B1�B9�BA�BI�BQ�BY�Ba�Bi�Bq�By�B�\)B�\)B�\)B��\B��\B��\B��\B�B�B��\B�\)B�\)B�\)B�\)B�\)B��\B��\Bď\Bȏ\B̏\BЏ\Bԏ\B؏\B܏\B��\B�\B�B�\B��\B�\B��\B��\C G�C{C.C.CG�C
G�CG�CG�CG�CG�CG�CG�CG�C.CG�CG�C G�C"G�C$G�C&G�C(G�C*G�C,G�C.G�C0G�C2G�C4G�C6G�C8G�C:G�C<G�C>G�C@G�CBG�CDG�CFG�CHG�CJG�CLG�CNG�CPG�CRG�CTG�CVG�CX.CZG�C\G�C^G�C`G�CbG�CdG�CfG�ChG�CjG�ClG�CnG�CpG�CrG�CtG�CvG�CxaHCzaHC|G�C~G�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�0�C�#�C�#�C�#�C�0�C�#�C�#�C�#�C�#�C�0�C�#�C�
C�#�C�#�C�#�C�#�C�#�C�
C�#�C�#�C�#�C�#�C�
C�#�C�#�C�#�C�#�C�#�C�#�C�0�C�0�C�#�C�#�C�
C�#�C�0�C�0�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�
C�#�C�#�C�
C�
C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�
C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�
C�#�C�#�C�#�C�#�C�
C�
C�#�C�#�C�#�C�#�C�#�C�0�C�#�C�#�C�#�C�
C�
=C�#�C�#�C�#�C�0�C�0�D RD ��D�D��D�D��D�D��DRD��D�D��D�D��D�D��DRD��D	�D	�RD
RD
�RD�D��D�D��DRD��D�D�RD�D��D�D��D�D��D�D��D�D�RD�D��D�D�RD�D��D�D��D�D��D�D��D�D��D�D��DRD�RD�D��D�D��D�D��D �D ��D!�D!��D"�D"�RD#RD#��D$�D$��D%�D%��D&�D&�RD'�D'��D(�D(��D)�D)�RD*RD*��D+�D+�RD,�D,��D-�D-�RD.�D.��D/�D/�RD0�D0��D1�D1��D2�D2��D3RD3�RD4RD4��D5�D5��D6RD6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO�RDP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU�RDVRDV�RDW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^�RD_RD_��D`�D`��DaRDa��Db�Db�RDc�Dc��Dd�Dd�RDe�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn�RDo�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Du�Du��DvRDv�RDwRDw��Dw޹Dy��D�E�D��
11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A���A���A���A���A���A���A���A�A���A�A�1A�JA�VA�bA�{A��A��A��A��A��A��A��A��A�$�A�(�A�(�A�33A�5?A�7LA�9XA�?}A�?}A�?}A�E�A�E�A�G�A�M�A�O�A�VA�\)A�v�A�1'A�VA��;Aӡ�A�dZA�7LA�oA��#AҮA҇+A�l�A�C�A��A��mA�A�1'A���A�M�A���A�v�A�=qA�oA�1'A��A�l�A�hsA���A�x�A��#AÇ+A��PA�z�A�-A�\)A��;A� �A���A��A�ffA���A��A��/A��`A�S�A�|�A�/A���A�r�A��RA�A�|�A�"�A��DA���A���A��PA��A�ȴA��A�l�A��uA���A�  A�t�A�&�A��PA��A�5?A�JA�p�A�33A��hA�(�A��!A���A�9XA�A�A�JA��HA�r�A�
=A��A��A�K�Ay��AuG�Al�A_�hA[�
AX�`AV$�ARQ�AM33AH�yAFbAE��AE%AB�`A@I�A<��A8�+A7x�A7\)A6�DA3�A0Q�A.�yA-��A-�A,M�A,  A+t�A+�A*�9A*�\A)�
A(z�A'�FA'dZA%hsA#�mA#%A"1'A!��A!;dAVAG�A$�A9XA��A�An�AK�A�A33A��A��AbA��A"�A�/A�+AdZAZA�wAp�A
�A
v�A
$�A
{A
1A
bA	��A	/A	
=A��A"�A��A�HAffA�A�A�`A�
A��At�A Q�@�v�@�+@���@��y@���@��7@�33@��@��@��@�@�b@���@�@��@��@�^5@��T@��@���@�|�@���@�9@�Ĝ@�9X@�-@܋D@�ȴ@��@���@�1@�E�@���@�|�@�;d@��H@ҟ�@�;d@�"�@�ff@���@мj@���@Ϯ@υ@�C�@���@���@�-@�-@͡�@���@��`@̋D@�dZ@ʰ!@�ȴ@���@���@ʏ\@�M�@�@�x�@�I�@ǍP@�S�@��@��T@��@Ĵ9@�Z@��@�@�M�@�J@���@���@�p�@��@��@�I�@��;@���@�V@�5?@�V@�-@��h@�x�@�`B@�7L@���@�bN@�K�@�@��H@��y@�{@��-@���@�`B@��@��w@�@�ȴ@��@���@�?}@��^@���@�`B@���@��@�Z@���@���@�M�@��@��@�@�X@�%@�dZ@�n�@�M�@�-@��T@�O�@��j@��@�ȴ@�=q@�@���@�O�@���@���@�r�@�bN@�bN@�Z@� �@�1@�  @��m@��@�;d@��@���@�E�@��@�p�@��@���@��9@�z�@�b@��@��P@�l�@�o@��R@�~�@��\@�n�@�$�@�@��h@���@�o@��+@�~�@�M�@�X@���@��@�bN@�I�@���@��@�o@���@���@�v�@�V@�M�@�ff@�@���@��-@���@�p�@�`B@��@��@��j@�Ĝ@��9@�bN@�I�@���@�33@�
=@�ȴ@�v�@�M�@�5?@���@��@��/@�r�@�Q�@�  @�|�@�K�@��@���@��!@�5?@��T@���@���@��@�hs@�X@�&�@�%@��@�(�@���@���@�r�@�r�@�j@�j@�Q�@��;@�t�@�C�@�C�@�l�@�"�@�o@���@�ȴ@�ff@���@�G�@�V@��`@��@�1'@�  @��;@��w@�t�@�C�@��H@�5?@��-@���@�r�@�(�@�ƨ@�ƨ@��@~��@i��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A���A���A���A���A���A���A���A�A���A�A�1A�JA�VA�bA�{A��A��A��A��A��A��A��A��A�$�A�(�A�(�A�33A�5?A�7LA�9XA�?}A�?}A�?}A�E�A�E�A�G�A�M�A�O�A�VA�\)A�v�A�1'A�VA��;Aӡ�A�dZA�7LA�oA��#AҮA҇+A�l�A�C�A��A��mA�A�1'A���A�M�A���A�v�A�=qA�oA�1'A��A�l�A�hsA���A�x�A��#AÇ+A��PA�z�A�-A�\)A��;A� �A���A��A�ffA���A��A��/A��`A�S�A�|�A�/A���A�r�A��RA�A�|�A�"�A��DA���A���A��PA��A�ȴA��A�l�A��uA���A�  A�t�A�&�A��PA��A�5?A�JA�p�A�33A��hA�(�A��!A���A�9XA�A�A�JA��HA�r�A�
=A��A��A�K�Ay��AuG�Al�A_�hA[�
AX�`AV$�ARQ�AM33AH�yAFbAE��AE%AB�`A@I�A<��A8�+A7x�A7\)A6�DA3�A0Q�A.�yA-��A-�A,M�A,  A+t�A+�A*�9A*�\A)�
A(z�A'�FA'dZA%hsA#�mA#%A"1'A!��A!;dAVAG�A$�A9XA��A�An�AK�A�A33A��A��AbA��A"�A�/A�+AdZAZA�wAp�A
�A
v�A
$�A
{A
1A
bA	��A	/A	
=A��A"�A��A�HAffA�A�A�`A�
A��At�A Q�@�v�@�+@���@��y@���@��7@�33@��@��@��@�@�b@���@�@��@��@�^5@��T@��@���@�|�@���@�9@�Ĝ@�9X@�-@܋D@�ȴ@��@���@�1@�E�@���@�|�@�;d@��H@ҟ�@�;d@�"�@�ff@���@мj@���@Ϯ@υ@�C�@���@���@�-@�-@͡�@���@��`@̋D@�dZ@ʰ!@�ȴ@���@���@ʏ\@�M�@�@�x�@�I�@ǍP@�S�@��@��T@��@Ĵ9@�Z@��@�@�M�@�J@���@���@�p�@��@��@�I�@��;@���@�V@�5?@�V@�-@��h@�x�@�`B@�7L@���@�bN@�K�@�@��H@��y@�{@��-@���@�`B@��@��w@�@�ȴ@��@���@�?}@��^@���@�`B@���@��@�Z@���@���@�M�@��@��@�@�X@�%@�dZ@�n�@�M�@�-@��T@�O�@��j@��@�ȴ@�=q@�@���@�O�@���@���@�r�@�bN@�bN@�Z@� �@�1@�  @��m@��@�;d@��@���@�E�@��@�p�@��@���@��9@�z�@�b@��@��P@�l�@�o@��R@�~�@��\@�n�@�$�@�@��h@���@�o@��+@�~�@�M�@�X@���@��@�bN@�I�@���@��@�o@���@���@�v�@�V@�M�@�ff@�@���@��-@���@�p�@�`B@��@��@��j@�Ĝ@��9@�bN@�I�@���@�33@�
=@�ȴ@�v�@�M�@�5?@���@��@��/@�r�@�Q�@�  @�|�@�K�@��@���@��!@�5?@��T@���@���@��@�hs@�X@�&�@�%@��@�(�@���@���@�r�@�r�@�j@�j@�Q�@��;@�t�@�C�@�C�@�l�@�"�@�o@���@�ȴ@�ff@���@�G�@�V@��`@��@�1'@�  @��;@��w@�t�@�C�@��H@�5?@��-@���@�r�@�(�@�ƨ@�ƨ@��@~��@i��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BB%BVBVBPBVBVBVBVBVBVBVBVBVBVBVBVBVBVBVBVBVBVBVBPBVBhBoBoBuB{B{B�B�B�B�B�B�B�B!�B0!BS�BVBZBaHBbNBdZBffBk�Bs�Bx�By�Bz�B|�B{�Bw�BjBbNBVBK�BI�BJ�BM�Bp�BĜB�mBB
=BDBVBhB"�B/B0!B@�BYBaHBw�B�JB�JB�=B�JB~�B`BBT�B@�B8RB2-B'�B{B	7B��B�B�BƨB�jB�-B��B��B�VB�1B}�Br�BjBYBI�B9XB&�BuB
��B
�B
�B
�;B
��B
�9B
��B
�VB
�B
�B
�B
x�B
n�B
aHB
XB
33B	��B	��B	y�B��B��B�-B��B�VB~�Bz�Bz�B{�By�Bw�Bu�By�B�B�%B�B�B�hB��B��B�B�B�-B�3B�?B�FB�LB�LB�XB�jB�qB�}BƨBƨBƨBƨBŢB��BɺBB�wB��B��BB��B�XB�B��B�B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�!B�3B�3B�-B�-B�3B�FB�RB�XB�XB�XB�qBƨB��B��B��BǮBǮB��B�/B�HB�5B�
B��B��B�
B�;B�ZB��B	B	+B	B	B��B��B	B��B�B�ZB�fB�mB�NB�5B�5B�BB�;B�;B�;B�5B�;B�HB�ZB�B�B��B��B��B��B	+B	PB	\B	bB	bB	bB	{B	{B	�B	�B	�B	�B	�B	!�B	#�B	%�B	(�B	+B	,B	,B	-B	0!B	5?B	7LB	7LB	6FB	6FB	6FB	7LB	7LB	9XB	:^B	;dB	?}B	G�B	L�B	M�B	Q�B	XB	^5B	_;B	`BB	cTB	dZB	ffB	k�B	l�B	l�B	m�B	n�B	m�B	l�B	q�B	r�B	u�B	y�B	|�B	~�B	�B	�B	�%B	�+B	�+B	�+B	�+B	�+B	�DB	�VB	�\B	�hB	�oB	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�'B	�3B	�9B	�FB	�LB	�LB	�RB	�XB	�^B	�^B	�dB	�dB	�jB	�wB	�}B	��B	��B	ÖB	ÖB	ÖB	B	�fB	�mB	�mB	�mB	�TB	�ZB	�ZB	�ZB	�`B	�fB	�fB	�`B	�`B	�`B	�fB	�fB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
+B
+B
+B
1B
1B
1B
	7B
1B
	7B

=B
DB
PB
\B
\B
\B
\B
bB
\B
\B
VB
VB
\B
bB
bB
hB
hB
hB
hB
\B
VB
JB
JB
JB
JB
bB
BB
!�B
.�22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222BB%BVBVBPBVBVBVBVBVBVBVBVBVBVBVBVBVBVBVBVBVBVBVBPBVBhBoBoBuB{B{B�B�B�B�B�B�B�B!�B0!BS�BVBZBaHBbNBdZBffBk�Bs�Bx�By�Bz�B|�B{�Bw�BjBbNBVBK�BI�BJ�BM�Bp�BĜB�mBB
=BDBVBhB"�B/B0!B@�BYBaHBw�B�JB�JB�=B�JB~�B`BBT�B@�B8RB2-B'�B{B	7B��B�B�BƨB�jB�-B��B��B�VB�1B}�Br�BjBYBI�B9XB&�BuB
��B
�B
�B
�;B
��B
�9B
��B
�VB
�B
�B
�B
x�B
n�B
aHB
XB
33B	��B	��B	y�B��B��B�-B��B�VB~�Bz�Bz�B{�By�Bw�Bu�By�B�B�%B�B�B�hB��B��B�B�B�-B�3B�?B�FB�LB�LB�XB�jB�qB�}BƨBƨBƨBƨBŢB��BɺBB�wB��B��BB��B�XB�B��B�B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�!B�3B�3B�-B�-B�3B�FB�RB�XB�XB�XB�qBƨB��B��B��BǮBǮB��B�/B�HB�5B�
B��B��B�
B�;B�ZB��B	B	+B	B	B��B��B	B��B�B�ZB�fB�mB�NB�5B�5B�BB�;B�;B�;B�5B�;B�HB�ZB�B�B��B��B��B��B	+B	PB	\B	bB	bB	bB	{B	{B	�B	�B	�B	�B	�B	!�B	#�B	%�B	(�B	+B	,B	,B	-B	0!B	5?B	7LB	7LB	6FB	6FB	6FB	7LB	7LB	9XB	:^B	;dB	?}B	G�B	L�B	M�B	Q�B	XB	^5B	_;B	`BB	cTB	dZB	ffB	k�B	l�B	l�B	m�B	n�B	m�B	l�B	q�B	r�B	u�B	y�B	|�B	~�B	�B	�B	�%B	�+B	�+B	�+B	�+B	�+B	�DB	�VB	�\B	�hB	�oB	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�'B	�3B	�9B	�FB	�LB	�LB	�RB	�XB	�^B	�^B	�dB	�dB	�jB	�wB	�}B	��B	��B	ÖB	ÖB	ÖB	B	�fB	�mB	�mB	�mB	�TB	�ZB	�ZB	�ZB	�`B	�fB	�fB	�`B	�`B	�`B	�fB	�fB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
+B
+B
+B
1B
1B
1B
	7B
1B
	7B

=B
DB
PB
\B
\B
\B
\B
bB
\B
\B
VB
VB
\B
bB
bB
hB
hB
hB
hB
\B
VB
JB
JB
JB
JB
bB
BB
!�B
.�22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.28 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005190534                              AO  ARCAADJP                                                                    20181005190534    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005190534  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005190534  QCF$                G�O�G�O�G�O�8000            