CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:17:00Z creation      
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
_FillValue                    �dArgo profile    3.1 1.2 19500101000000  20181005191700  20181005191700  5904954 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               3A   AO  6557                            2B  A   APEX                            7468                            062512                          846 @׼$���1   @׼%ffy@5Z�G�{�c��\)1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      3A   A   A   @9��@�  @�  A   A   A@  A^ffA�  A�  A�  A�  A�33A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BPffBX  B`ffBh  Bp  Bx  B�  B�  B�  B�  B�33B�33B�  B�  B�  B�  B���B�  B�  B�  B���B�  B�  B�33B�  B�  B�  B�  B���B���B�  B�  B�  B�  B�33B�33B�33B�  C   C  C  C  C  C
  C  C  C�fC�fC  C  C  C  C  C  C   C"  C$  C%�fC'�fC*  C,  C.  C0  C2  C4  C5�fC8  C:  C<  C>�C@  CB  CD  CF  Cv  Cx  Cz�C|�C~�C��C�  C�  C��C�  C��3C��C�  C��3C��3C�  C�  C�  C��3C��3C��3C�  C�  C�  C��C��C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C��C�  C�  C��C�  C�  C�  C�  C��C�  C�  C��3C�  C�  C��C��3C�  C�  C�  C�  C�  C�  C��3C�  C�  C��3C�  C��C��C��C��C��3C�  C��C�  C��3C��3C��fC��3C��C��C��C��C�  C��C��C��C��C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C��3C�  C��C��C�  C�  C�  C��C�  C��3C��3C��3C�  C��C�  C�  C��C��C��3C��3C�  C�  C��3C��3C��C��C�  C��C��C�  C��C��C��3D � DfD�fD��D� D  D� D  D� D  D� D  D� D��Dy�D  D� D	  D	�fD
  D
y�D
��Ds3D  D�fDfD�fDfD� DfD�fD  Dy�D��Dy�D��Dy�D��Dy�D�3Dy�D  D� DfD� DfD�fD��Dy�D  D� DfD� D�3Dy�D  D� D  D�fD  D� D  D�fD   D y�D ��D!� D"fD"�fD#fD#�fD$fD$�fD%  D%� D&  D&�fD'  D'� D(  D(� D(�3D)y�D)��D*y�D+  D+� D,  D,�fD-fD-�fD.  D.y�D.��D/y�D/��D0y�D1  D1�fD2fD2� D2��D3y�D4  D4�fD5  D5� D6  D6y�D7  D7� D8  D8� D9  D9y�D9��D:y�D:��D;y�D;��D<y�D=  D=�fD>fD>� D?  D?��D@�D@�fDAfDA� DB  DBy�DB��DC� DDfDD�fDEfDE� DF  DF�fDG  DG�fDHfDH�fDI  DI� DJfDJy�DJ��DK� DLfDL� DM  DM� DNfDN� DN��DOy�DP  DP� DQ  DQ�fDR  DR�fDSfDS� DS��DTy�DU  DU� DV  DVy�DV��DWy�DX  DX�fDYfDY� DY��DZ� D[fD[�fD\  D\� D\��D]� D]��D^y�D_fD_� D`  D`� Da  Day�Da��Db�fDcfDc� Dc��Dd� DefDe�fDf  Dfy�Df��Dgy�Dg��Dhy�Di  Di� Dj  Dj� Dk  Dk� Dk��Dly�Dl��Dm�fDnfDn� Dn��Doy�Dp  Dp� Dq  Dqy�Dq�3Dry�Ds  Ds� Dt  Dt� Du  Duy�Dv  Dv�fDwfDw�fDwٚDy�D�AHD���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@K�@���@���Az�A$z�ADz�Ab�GA�=qA�=qA�=qA�=qA�p�A�=qA�=qA�=qB�B	�B�B�B!�B)�B1�B9�BA�BI�BQ�BY�Ba�Bi�Bq�By�B��\B��\B��\B��\B�B�B��\B��\B��\B��\B�\)B��\B��\B��\B�\)B��\B��\B�Bȏ\B̏\BЏ\Bԏ\B�\)B�\)B��\B�\B�\B�\B�B�B�B��\C G�CG�CG�CG�CG�C
G�CG�CG�C.C.CG�CG�CG�CG�CG�CG�C G�C"G�C$G�C&.C(.C*G�C,G�C.G�C0G�C2G�C4G�C6.C8G�C:G�C<G�C>aHC@G�CBG�CDG�CFG�CvG�CxG�CzaHC|aHC~aHC�0�C�#�C�#�C�0�C�#�C�
C�0�C�#�C�
C�
C�#�C�#�C�#�C�
C�
C�
C�#�C�#�C�#�C�0�C�0�C�#�C�#�C�0�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�0�C�#�C�#�C�#�C�#�C�#�C�0�C�#�C�#�C�0�C�#�C�#�C�#�C�#�C�0�C�#�C�#�C�
C�#�C�#�C�0�C�
C�#�C�#�C�#�C�#�C�#�C�#�C�
C�#�C�#�C�
C�#�C�=qC�0�C�0�C�0�C�
C�#�C�0�C�#�C�
C�
C�
=C�
C�0�C�0�C�0�C�0�C�#�C�0�C�0�C�0�C�=qC�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�
C�
C�
C�#�C�0�C�0�C�#�C�#�C�#�C�0�C�#�C�
C�
C�
C�#�C�0�C�#�C�#�C�0�C�0�C�
C�
C�#�C�#�C�
C�
C�0�C�0�C�#�C�0�C�0�C�#�C�0�C�0�D �D ��DRD�RD�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	�RD
�D
��D�D�D�D�RDRD�RDRD��DRD�RD�D��D�D��D�D��D�D��DD��D�D��DRD��DRD�RD�D��D�D��DRD��DD��D�D��D�D�RD�D��D�D�RD �D ��D!�D!��D"RD"�RD#RD#�RD$RD$�RD%�D%��D&�D&�RD'�D'��D(�D(��D)D)��D*�D*��D+�D+��D,�D,�RD-RD-�RD.�D.��D/�D/��D0�D0��D1�D1�RD2RD2��D3�D3��D4�D4�RD5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=�RD>RD>��D?�D?��D@�D@�RDARDA��DB�DB��DC�DC��DDRDD�RDERDE��DF�DF�RDG�DG�RDHRDH�RDI�DI��DJRDJ��DK�DK��DLRDL��DM�DM��DNRDN��DO�DO��DP�DP��DQ�DQ�RDR�DR�RDSRDS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX�RDYRDY��DZ�DZ��D[RD[�RD\�D\��D]�D]��D^�D^��D_RD_��D`�D`��Da�Da��Db�Db�RDcRDc��Dd�Dd��DeRDe�RDf�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm�RDnRDn��Do�Do��Dp�Dp��Dq�Dq��DrDr��Ds�Ds��Dt�Dt��Du�Du��Dv�Dv�RDwRDw�RDw�Dy�D�J>D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�AҬAҶFAҸRA�ĜA�ĜA�ĜA�A�ĜA�ȴA�ȴA�A�ƨA�AҴ9Aҟ�A�r�A��A��
A�z�A�`BA�VA���A�ȴAв-AЧ�AЇ+A�ffA�VA�&�A�ĜAΟ�A�ĜAͥ�A̩�A˙�A�33Aȡ�A�p�A���A�ZA��#A�+A��DA���A��A�XA�=qA��A�%A��RA��jA�`BA�VA�ZA���A�A�A�5?A�bA��A���A�ffA��`A��^A�\)A��RA���A�C�A�dZA�1A���A�dZA��A��#A� �A��;A��A��`A��A���A�oA���A��jA�5?A�G�A�ĜA��-A���A���A��9A���A�(�A��A���A�33A���A��!A��`A��A�l�AcS�A_��A[�hAW7LASƨAR��ANȴAM%AK�
AJ�AGAF1'AEhsACoAA��A@��A>ffA=A:��A8��A5�hA4bA3�wA3dZA2��A2=qA1��A0�uA-�;A+��A+oA*��A*ZA)�A&�DA%`BA$��A$Q�A#�A!��A ZA�A33A�`A1'A&�A{A�A�mA�TA�wA�A33A�AA�A�AAffA|�A��A��A�/A��A�A �A�wA
ĜA
ffA	��A�RA��A��A/A�
A��A�hA%A��AjA�A�AƨA ȴA z�A @�o@�-@�^5@��#@�Z@�Z@�(�@�\)@��y@��+@�9X@�hs@�V@�V@���@��@�(�@�1@�@�+@@��T@���@���@�w@�/@�t�@��y@��@۶F@�;d@�33@���@�9X@��@�t�@֧�@���@�z�@ӍP@Ұ!@�^5@��@щ7@�/@Ь@�1@ΰ!@ͺ^@�/@˶F@�n�@�@�hs@ȴ9@��;@�ƨ@ǶF@Ǖ�@��@�&�@�(�@å�@�+@�n�@��@�x�@�/@��/@�A�@� �@�b@�l�@�=q@�=q@��@���@���@��@�S�@�C�@��@�=q@��h@��@���@��`@���@�Ĝ@�9X@��@��@��;@�\)@�ȴ@���@�ff@�5?@���@���@���@�hs@��@��/@�bN@�  @��P@�\)@�;d@�@��y@��!@�n�@�E�@�J@���@�?}@�V@��/@��j@�z�@�  @�ƨ@��F@���@���@�dZ@�;d@�+@���@��@��H@�v�@�x�@���@��P@���@�V@�V@�V@�=q@�5?@�5?@�5?@�5?@��@���@�J@��@��@���@�Q�@��@�ƨ@�l�@���@��y@���@��!@���@�^5@�-@�$�@��@��^@��7@�ƨ@�+@�V@�@�x�@�X@�p�@�p�@��/@�z�@�(�@�(�@���@��
@�l�@��@��R@��\@�^5@��T@���@�O�@�G�@�G�@�?}@�7L@�7L@���@�z�@���@��@��P@�\)@�33@�o@��\@���@�hs@��`@���@�j@� �@�ƨ@�\)@�33@���@�^5@�M�@�=q@��@�@��T@��-@�?}@��@��@�%@�Ĝ@�z�@�1'@��@�b@�1@�  @���@��w@���@�t�@�K�@�C�@�"�@��@��y@���@�ȴ@��R@��!@���@��\@�^5@���@���@�p�@�hs@�`B@�O�@�?}@�/@��@���@���@���@��@�Q�@�b@�  @��@���@��@��@�C�@�@��@��y@��@���@��@��-@���@���@��h@�/@���@��@��@�1@���@�t�@�dZ@�33@�@���@���@��@}��@kE911111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111AҬAҶFAҸRA�ĜA�ĜA�ĜA�A�ĜA�ȴA�ȴA�A�ƨA�AҴ9Aҟ�A�r�A��A��
A�z�A�`BA�VA���A�ȴAв-AЧ�AЇ+A�ffA�VA�&�A�ĜAΟ�A�ĜAͥ�A̩�A˙�A�33Aȡ�A�p�A���A�ZA��#A�+A��DA���A��A�XA�=qA��A�%A��RA��jA�`BA�VA�ZA���A�A�A�5?A�bA��A���A�ffA��`A��^A�\)A��RA���A�C�A�dZA�1A���A�dZA��A��#A� �A��;A��A��`A��A���A�oA���A��jA�5?A�G�A�ĜA��-A���A���A��9A���A�(�A��A���A�33A���A��!A��`A��A�l�AcS�A_��A[�hAW7LASƨAR��ANȴAM%AK�
AJ�AGAF1'AEhsACoAA��A@��A>ffA=A:��A8��A5�hA4bA3�wA3dZA2��A2=qA1��A0�uA-�;A+��A+oA*��A*ZA)�A&�DA%`BA$��A$Q�A#�A!��A ZA�A33A�`A1'A&�A{A�A�mA�TA�wA�A33A�AA�A�AAffA|�A��A��A�/A��A�A �A�wA
ĜA
ffA	��A�RA��A��A/A�
A��A�hA%A��AjA�A�AƨA ȴA z�A @�o@�-@�^5@��#@�Z@�Z@�(�@�\)@��y@��+@�9X@�hs@�V@�V@���@��@�(�@�1@�@�+@@��T@���@���@�w@�/@�t�@��y@��@۶F@�;d@�33@���@�9X@��@�t�@֧�@���@�z�@ӍP@Ұ!@�^5@��@щ7@�/@Ь@�1@ΰ!@ͺ^@�/@˶F@�n�@�@�hs@ȴ9@��;@�ƨ@ǶF@Ǖ�@��@�&�@�(�@å�@�+@�n�@��@�x�@�/@��/@�A�@� �@�b@�l�@�=q@�=q@��@���@���@��@�S�@�C�@��@�=q@��h@��@���@��`@���@�Ĝ@�9X@��@��@��;@�\)@�ȴ@���@�ff@�5?@���@���@���@�hs@��@��/@�bN@�  @��P@�\)@�;d@�@��y@��!@�n�@�E�@�J@���@�?}@�V@��/@��j@�z�@�  @�ƨ@��F@���@���@�dZ@�;d@�+@���@��@��H@�v�@�x�@���@��P@���@�V@�V@�V@�=q@�5?@�5?@�5?@�5?@��@���@�J@��@��@���@�Q�@��@�ƨ@�l�@���@��y@���@��!@���@�^5@�-@�$�@��@��^@��7@�ƨ@�+@�V@�@�x�@�X@�p�@�p�@��/@�z�@�(�@�(�@���@��
@�l�@��@��R@��\@�^5@��T@���@�O�@�G�@�G�@�?}@�7L@�7L@���@�z�@���@��@��P@�\)@�33@�o@��\@���@�hs@��`@���@�j@� �@�ƨ@�\)@�33@���@�^5@�M�@�=q@��@�@��T@��-@�?}@��@��@�%@�Ĝ@�z�@�1'@��@�b@�1@�  @���@��w@���@�t�@�K�@�C�@�"�@��@��y@���@�ȴ@��R@��!@���@��\@�^5@���@���@�p�@�hs@�`B@�O�@�?}@�/@��@���@���@���@��@�Q�@�b@�  @��@���@��@��@�C�@�@��@��y@��@���@��@��-@���@���@��h@�/@���@��@��@�1@���@�t�@�dZ@�33@�@���@���@��@}��@kE911111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�uB�uB�uB�uB�oB�uB�uB�uB�uB�{B��B��B��B��B��B��B�B�?BBĜBĜBŢBŢBǮBǮBƨBŢBĜBĜBƨB��B�5B�5B�yB��B+B �B:^BA�BP�BdZBiyBr�B{�B�bB��B�B�'B�jB��B�)B�BB�HB�sB��BB%BJBVBoBVBDB
=BbB\B
=B  B�B�B�fB�NB�BŢB�RB�XB�FB�B��B�B��B��B�uB��B��B�bB�B{�Br�Bl�Be`BdZB^5BG�B�BB�BBɺB�{B{�B	�+B	l�B	P�B	<jB	,B	#�B	�B	PB	1B��B��B�B�B�ZB�5B�B��B��BǮB��B�XB�9B�3B�'B�B�B�B��B��B��B��B��B��B�{B�oB�oB�oB�hB�bB�oB�oB�hB�\B�VB�\B�{B��B��B��B��B��B��B��B�{B�uB�uB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B��B��B�B�!B�!B�'B�'B�-B�-B�-B�-B�-B�-B�-B�'B�!B�B�B�!B�'B�FB�LB�FB�LB�XB�jB�jB�dB�dB�wBÖBǮB��B��B��B��B��B�B�B�)B�/B�/B�BB�ZB�`B�sB�B�B�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B	  B	B	
=B	DB	VB	\B	hB	{B	�B	�B	�B	�B	!�B	%�B	%�B	&�B	&�B	'�B	+B	.B	.B	/B	0!B	49B	7LB	:^B	<jB	A�B	D�B	E�B	F�B	H�B	J�B	N�B	Q�B	T�B	W
B	XB	YB	ZB	[#B	]/B	_;B	bNB	ffB	k�B	m�B	o�B	p�B	s�B	x�B	{�B	{�B	{�B	|�B	}�B	�B	�B	�B	�B	�%B	�7B	�\B	�hB	�hB	�uB	�{B	�{B	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�B	�B	�B	�B	�-B	�9B	�?B	�RB	�XB	�LB	�RB	�XB	�^B	�dB	�jB	�qB	�wB	�}B	�}B	�}B	�}B	��B	B	ÖB	ÖB	ÖB	ÖB	ÖB	ĜB	ƨB	ȴB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�#B	�/B	�/B	�/B	�5B	�5B	�5B	�;B	�HB	�HB	�HB	�HB	�NB	�TB	�`B	�`B	�`B	�fB	�fB	�fB	�mB	�mB	�sB	�yB	�yB	�yB	�yB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
 iB
?B
'�22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222B�uB�uB�uB�uB�oB�uB�uB�uB�uB�{B��B��B��B��B��B��B�B�?BBĜBĜBŢBŢBǮBǮBƨBŢBĜBĜBƨB��B�5B�5B�yB��B+B �B:^BA�BP�BdZBiyBr�B{�B�bB��B�B�'B�jB��B�)B�BB�HB�sB��BB%BJBVBoBVBDB
=BbB\B
=B  B�B�B�fB�NB�BŢB�RB�XB�FB�B��B�B��B��B�uB��B��B�bB�B{�Br�Bl�Be`BdZB^5BG�B�BB�BBɺB�{B{�B	�+B	l�B	P�B	<jB	,B	#�B	�B	PB	1B��B��B�B�B�ZB�5B�B��B��BǮB��B�XB�9B�3B�'B�B�B�B��B��B��B��B��B��B�{B�oB�oB�oB�hB�bB�oB�oB�hB�\B�VB�\B�{B��B��B��B��B��B��B��B�{B�uB�uB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B��B��B�B�!B�!B�'B�'B�-B�-B�-B�-B�-B�-B�-B�'B�!B�B�B�!B�'B�FB�LB�FB�LB�XB�jB�jB�dB�dB�wBÖBǮB��B��B��B��B��B�B�B�)B�/B�/B�BB�ZB�`B�sB�B�B�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B	  B	B	
=B	DB	VB	\B	hB	{B	�B	�B	�B	�B	!�B	%�B	%�B	&�B	&�B	'�B	+B	.B	.B	/B	0!B	49B	7LB	:^B	<jB	A�B	D�B	E�B	F�B	H�B	J�B	N�B	Q�B	T�B	W
B	XB	YB	ZB	[#B	]/B	_;B	bNB	ffB	k�B	m�B	o�B	p�B	s�B	x�B	{�B	{�B	{�B	|�B	}�B	�B	�B	�B	�B	�%B	�7B	�\B	�hB	�hB	�uB	�{B	�{B	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�B	�B	�B	�B	�-B	�9B	�?B	�RB	�XB	�LB	�RB	�XB	�^B	�dB	�jB	�qB	�wB	�}B	�}B	�}B	�}B	��B	B	ÖB	ÖB	ÖB	ÖB	ÖB	ĜB	ƨB	ȴB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�#B	�/B	�/B	�/B	�5B	�5B	�5B	�;B	�HB	�HB	�HB	�HB	�NB	�TB	�`B	�`B	�`B	�fB	�fB	�fB	�mB	�mB	�sB	�yB	�yB	�yB	�yB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
 iB
?B
'�22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.28 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005191700                              AO  ARCAADJP                                                                    20181005191700    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005191700  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005191700  QCF$                G�O�G�O�G�O�8000            