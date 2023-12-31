CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:06:01Z creation      
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
_FillValue                 �  A4   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C(   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  T�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \h   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ^\   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  f    TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  h   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  o�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  w�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  y�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �T   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �H   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �<   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �<   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �<   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �<   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �h   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �l   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �p   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �t   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �x   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20181005190601  20181005190601  5904952 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6431                            2B  A   APEX                            7466                            062512                          846 @��)�P{�1   @��*q�1p@1g+I��c�ě��T1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �A   A   A   @�ff@�  A   A   AA��A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B��B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C7�fC:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C_�fCb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�fC�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C��3C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C��C��C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C��C�  C�  C�  C�  C�  C�  D   D � D ��Dy�D  D�fD  D� D  D� D  D� D  Dy�D��D� D  D� D��D	y�D	��D
y�D
��D� D  Dy�D  D� D  D� D  D� DfD� D  D�fD  D� D  D� D  D� D��D� D��Dy�D  D� D  D�fD  D� D  D� D  D� D��D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%�fD&  D&� D&��D'� D(  D(� D)  D)� D*fD*� D+  D+� D,  D,y�D-  D-� D.  D.� D/  D/� D0  D0y�D1  D1� D2  D2� D3  D3� D4  D4� D5  D5y�D6  D6� D7  D7� D8  D8� D9  D9y�D9��D:� D;  D;y�D;��D<� D=  D=� D>  D>� D?  D?� D@  D@y�D@��DA� DB  DB�fDC  DCy�DD  DD�fDEfDE� DE��DF� DGfDG�fDHfDH�fDI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQy�DR  DR�fDSfDS� DT  DT� DU  DU� DU��DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D[��D\y�D\��D]y�D]��D^� D_fD_� D_��D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De�fDf  Df� Dg  Dg� Dh  Dh� Di  Diy�Dj  Dj� Dj��Dk� DlfDl�fDmfDm� Dn  Dn� Do  Do� Dp  Dpy�Dp��Dq� Dr  Dr� Ds  Ds� DtfDt� Du  Duy�Dv  Dv� Dw  Dw� Dw� DyxRD�C311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�\)@���Az�A$z�AF{Adz�A�=qA�=qA�=qA�=qA�=qA�=qA�=qA�=qB�B	�B�RB�B!�B)�B1�B9�BA�BI�BQ�BY�Ba�Bi�Bq�By�B��\B��\B��\B��\B��\B��\B��\B��\B��\B��\B��\B�B�B��\B��\B��\B��\Bď\Bȏ\B̏\BЏ\Bԏ\B؏\B܏\B��\B�\B�\B�\B��\B�\B��\B��\C G�CG�CG�CG�CG�C
G�CG�CG�CG�CG�CG�CG�CG�CG�CG�CG�C G�C"G�C$G�C&G�C(G�C*G�C,G�C.G�C0G�C2G�C4G�C6G�C8.C:G�C<G�C>G�C@G�CBG�CDG�CFG�CHG�CJG�CLG�CNG�CPG�CRG�CTG�CVG�CXG�CZG�C\G�C^G�C`.CbG�CdG�CfG�ChG�CjG�ClG�CnG�CpG�CrG�CtG�CvG�CxG�CzG�C|G�C~G�C�
C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�0�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�
C�
C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�0�C�0�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�0�C�#�C�#�C�#�C�
C�#�C�#�C�#�C�0�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�0�C�0�C�#�C�#�C�#�C�0�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�
C�#�C�0�C�0�C�#�C�#�C�#�C�#�C�0�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�0�C�0�C�0�C�#�C�#�C�#�C�#�C�#�C�#�D �D ��D�D��D�D�RD�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��DRD��D�D�RD�D��D�D��D�D��D�D��D�D��D�D��D�D�RD�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%�RD&�D&��D'�D'��D(�D(��D)�D)��D*RD*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB�RDC�DC��DD�DD�RDERDE��DF�DF��DGRDG�RDHRDH�RDI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR�RDSRDS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_RD_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De�RDf�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��DlRDl�RDmRDm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��DtRDt��Du�Du��Dv�Dv��Dw�Dw��Dw��Dy�>D�L)11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�AɾwAɾwAɺ^Aɺ^AɶFAɺ^Aɥ�A�dZA���A���A�ȴA���A��A��AǴ9A�M�A��A���A�^5Aź^Aĺ^A�^5AĬAĶFA�M�AĬA�r�A�(�A��;AąA�A�Aŉ7A�l�A�E�A���A�%A���A�`BA�$�A��A��HA�A���A��A�S�A�5?A�
=A���A�33A���A�%A��`A��`A��!A�|�A��hA�A�A�VA��yA�^5A���A�l�A��
A��TA�A�  A� �A�=qA�A�x�A�
=A���A��A�
=A���A�Q�A��A�;dA��mA�M�A�  A��A���A��jA��uA��#A��^A�$�A�r�A�bA��A���A��A�$�A�&�A��A��HA���A��HA��A�5?A���A��A�+A�A�|�A�%A�~�A��A�A�{Az�`AyoAwK�AvM�As�mArn�Aq�Ap{Am�Aj�\AeVA]dZAX�AV��ATȴARĜAP�/AO�AOS�AMC�AH�AC��A@��A>��A:��A9��A7��A4��A2ffA1��A1C�A0~�A/�^A//A.A,��A*�yA*A�A)|�A(��A'��A'7LA&I�A$�A"��A�-A�A�A�A��A��A��A�A5?AO�AffA;dAbNAl�A �A+Ar�A��A��A��A1'A�A�A-AM�A
=AS�AK�A
5?A�!A��AVA��A{A�A��AVA33A v�A r�A jA I�@���@��H@��R@���@�t�@�o@��A VA;dA\)A �A r�A A�A I�A -@��P@�^5@�p�@�b@�"�@��\@�p�@�%@��/@���@���@�J@�n�@�  @�C�@�{@��@�+@�|�@�  @@�
=@�M�@�7@�j@�+@ꟾ@陚@�&�@�A�@���@��@�{@�E�@�S�@睲@柾@�E�@���@�Ĝ@�z�@䛦@� �@�E�@�@��@�K�@�^5@���@��@��@��;@��
@��
@۾w@�|�@ڏ\@��@�7L@�(�@��@�|�@ָR@�n�@ԋD@Ӆ@҇+@�E�@�bN@�t�@�n�@Ͳ-@ͺ^@�p�@�  @ˍP@��@���@���@�Z@�z�@��@ʰ!@�$�@�-@���@�&�@ȓu@���@ǅ@Ɵ�@��T@�X@�9X@�K�@�ff@��@�`B@���@��@�z�@�(�@��F@���@�{@���@�p�@�`B@�V@�j@�1@�ƨ@��@��@���@��+@�7L@��u@�b@�o@�v�@��@�Ĝ@�  @�C�@�ȴ@���@�E�@��-@�`B@��@��9@��D@�A�@�1'@� �@��@�S�@�t�@�|�@�|�@�S�@���@��\@�E�@��^@�p�@�?}@�V@�j@��;@���@�l�@�;d@�@��@��@���@���@���@�n�@�5?@��T@�x�@��@���@��9@���@��@�Z@�9X@��;@��@��@�"�@�
=@��R@��@���@���@��@�p�@�O�@��@���@���@�z�@�9X@��@��P@�\)@���@���@��R@���@�E�@��@��7@��u@��@��@��w@���@�\)@�S�@�l�@�33@�@��!@�@���@��-@���@�x�@�?}@��@�bN@�b@��@��
@���@��
@���@���@�;d@�K�@�C�@�+@�"�@�"�@���@��+@�v�@�ff@��T@���@���@�z�@�bN@�Z@�A�@�  @���@�l�@���@�M�@�=q@��@��#@���@���@���@�X@�Ĝ@���@��D@�Z@�  @��F@�t�@�33@�@��R@��+@�V@�5?@�$�@�@��@�O�@���@��j@���@�j@�  @���@���@�\)@��@��H@��R@���@��@��g11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   AɾwAɾwAɺ^Aɺ^AɶFAɺ^Aɥ�A�dZA���A���A�ȴA���A��A��AǴ9A�M�A��A���A�^5Aź^Aĺ^A�^5AĬAĶFA�M�AĬA�r�A�(�A��;AąA�A�Aŉ7A�l�A�E�A���A�%A���A�`BA�$�A��A��HA�A���A��A�S�A�5?A�
=A���A�33A���A�%A��`A��`A��!A�|�A��hA�A�A�VA��yA�^5A���A�l�A��
A��TA�A�  A� �A�=qA�A�x�A�
=A���A��A�
=A���A�Q�A��A�;dA��mA�M�A�  A��A���A��jA��uA��#A��^A�$�A�r�A�bA��A���A��A�$�A�&�A��A��HA���A��HA��A�5?A���A��A�+A�A�|�A�%A�~�A��A�A�{Az�`AyoAwK�AvM�As�mArn�Aq�Ap{Am�Aj�\AeVA]dZAX�AV��ATȴARĜAP�/AO�AOS�AMC�AH�AC��A@��A>��A:��A9��A7��A4��A2ffA1��A1C�A0~�A/�^A//A.A,��A*�yA*A�A)|�A(��A'��A'7LA&I�A$�A"��A�-A�A�A�A��A��A��A�A5?AO�AffA;dAbNAl�A �A+Ar�A��A��A��A1'A�A�A-AM�A
=AS�AK�A
5?A�!A��AVA��A{A�A��AVA33A v�A r�A jA I�@���@��H@��R@���@�t�@�o@��A VA;dA\)A �A r�A A�A I�A -@��P@�^5@�p�@�b@�"�@��\@�p�@�%@��/@���@���@�J@�n�@�  @�C�@�{@��@�+@�|�@�  @@�
=@�M�@�7@�j@�+@ꟾ@陚@�&�@�A�@���@��@�{@�E�@�S�@睲@柾@�E�@���@�Ĝ@�z�@䛦@� �@�E�@�@��@�K�@�^5@���@��@��@��;@��
@��
@۾w@�|�@ڏ\@��@�7L@�(�@��@�|�@ָR@�n�@ԋD@Ӆ@҇+@�E�@�bN@�t�@�n�@Ͳ-@ͺ^@�p�@�  @ˍP@��@���@���@�Z@�z�@��@ʰ!@�$�@�-@���@�&�@ȓu@���@ǅ@Ɵ�@��T@�X@�9X@�K�@�ff@��@�`B@���@��@�z�@�(�@��F@���@�{@���@�p�@�`B@�V@�j@�1@�ƨ@��@��@���@��+@�7L@��u@�b@�o@�v�@��@�Ĝ@�  @�C�@�ȴ@���@�E�@��-@�`B@��@��9@��D@�A�@�1'@� �@��@�S�@�t�@�|�@�|�@�S�@���@��\@�E�@��^@�p�@�?}@�V@�j@��;@���@�l�@�;d@�@��@��@���@���@���@�n�@�5?@��T@�x�@��@���@��9@���@��@�Z@�9X@��;@��@��@�"�@�
=@��R@��@���@���@��@�p�@�O�@��@���@���@�z�@�9X@��@��P@�\)@���@���@��R@���@�E�@��@��7@��u@��@��@��w@���@�\)@�S�@�l�@�33@�@��!@�@���@��-@���@�x�@�?}@��@�bN@�b@��@��
@���@��
@���@���@�;d@�K�@�C�@�+@�"�@�"�@���@��+@�v�@�ff@��T@���@���@�z�@�bN@�Z@�A�@�  @���@�l�@���@�M�@�=q@��@��#@���@���@���@�X@�Ĝ@���@��D@�Z@�  @��F@�t�@�33@�@��R@��+@�V@�5?@�$�@�@��@�O�@���@��j@���@�j@�  @���@���@�\)@��@��H@��R@���@��@��g11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BǮBǮBƨBƨBȴBɺBɺB��B��B	B	{B	 �B	�B	<jB	[#B	q�B	�VB	�}B	�B
	7B
�B
<jB
dZB
� B
�1B
�'B
�jB
ÖB
��B�BbNBx�B�oB��B� B]/B2-B.B49B8RB8RB9XB:^B=qBB�BH�BM�BN�BR�BS�B_;B��B��BŢB��B�B�ZB�yB�mB�TB��B�B�yB�`B�NB�ZB�B�B
=B��B�B�B�B�B�mB�fB�ZB�HB�5BÖB��B��B{�BcTBH�B:^B"�B+B	7B	7B%�B@�B<jB#�BB
��B
�B
�)B
�qB
��B
�hB
{�B
iyB
O�B
%�B
oB
DB
B	��B	��B	�;B	�LB	�B	��B	��B	�\B	�%B	~�B	s�B	bNB	E�B	�B�yBɺB��B��B�B�sB��B��B	hB	VB�B�
BǮB�3B�'B�'B�'B�3B�9B�9B�3B�3B�-B�-B�?B�^B�}BȴB��B��B�B�fB�5B��B�dB�'B�9B�?B�FB�?B�FB�RB�dB�jB�jB�qB�wB��BÖBĜBƨB��B��B��B�B�)B�)B�/B�TB�B	B	oB	�B��B��B��B	B	VB	�B	$�B	&�B	#�B	&�B	,B	.B	.B	33B	49B	9XB	;dB	O�B	]/B	^5B	q�B	�%B	�7B	�JB	�PB	�bB	�oB	�hB	�bB	�\B	�JB	�7B	�%B	�B	� B	|�B	{�B	y�B	n�B	aHB	S�B	J�B	H�B	H�B	bNB	y�B	{�B	� B	� B	� B	�B	�B	�B	�B	�+B	�=B	�7B	�=B	�PB	�hB	�uB	��B	��B	��B	�B	�B	��B	��B	��B	��B	��B	��B	�3B	�9B	�B	�B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	�B	�B	�B	�B	�B	�?B	�3B	�-B	�3B	�9B	�RB	�FB	�LB	�LB	�RB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	��B	��B	��B	�B	�B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�B	�B	�B	�B	�#B	�/B	�BB	�HB	�BB	�BB	�NB	�NB	�NB	�NB	�HB	�HB	�NB	�TB	�ZB	�`B	�`B	�fB	�fB	�mB	�fB	�fB	�fB	�mB	�mB	�mB	�mB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
%B
%B
%B
1B
	7B
	7B
	7B

=B
DB
JB
VB
VB
VB
VB
\B
\B
\B
\B
\B
bB
bB
bB
bB
hB
bB
bB
bB
bB
bB
bB
bB
bB
hB
uB
uB
uB
uB
uB
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
#�22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222   BǮBǮBƨBƨBȴBɺBɺB��B��B	B	{B	 �B	�B	<jB	[#B	q�B	�VB	�}B	�B
	7B
�B
<jB
dZB
� B
�1B
�'B
�jB
ÖB
��B�BbNBx�B�oB��B� B]/B2-B.B49B8RB8RB9XB:^B=qBB�BH�BM�BN�BR�BS�B_;B��B��BŢB��B�B�ZB�yB�mB�TB��B�B�yB�`B�NB�ZB�B�B
=B��B�B�B�B�B�mB�fB�ZB�HB�5BÖB��B��B{�BcTBH�B:^B"�B+B	7B	7B%�B@�B<jB#�BB
��B
�B
�)B
�qB
��B
�hB
{�B
iyB
O�B
%�B
oB
DB
B	��B	��B	�;B	�LB	�B	��B	��B	�\B	�%B	~�B	s�B	bNB	E�B	�B�yBɺB��B��B�B�sB��B��B	hB	VB�B�
BǮB�3B�'B�'B�'B�3B�9B�9B�3B�3B�-B�-B�?B�^B�}BȴB��B��B�B�fB�5B��B�dB�'B�9B�?B�FB�?B�FB�RB�dB�jB�jB�qB�wB��BÖBĜBƨB��B��B��B�B�)B�)B�/B�TB�B	B	oB	�B��B��B��B	B	VB	�B	$�B	&�B	#�B	&�B	,B	.B	.B	33B	49B	9XB	;dB	O�B	]/B	^5B	q�B	�%B	�7B	�JB	�PB	�bB	�oB	�hB	�bB	�\B	�JB	�7B	�%B	�B	� B	|�B	{�B	y�B	n�B	aHB	S�B	J�B	H�B	H�B	bNB	y�B	{�B	� B	� B	� B	�B	�B	�B	�B	�+B	�=B	�7B	�=B	�PB	�hB	�uB	��B	��B	��B	�B	�B	��B	��B	��B	��B	��B	��B	�3B	�9B	�B	�B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	�B	�B	�B	�B	�B	�?B	�3B	�-B	�3B	�9B	�RB	�FB	�LB	�LB	�RB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	��B	��B	��B	�B	�B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�B	�B	�B	�B	�#B	�/B	�BB	�HB	�BB	�BB	�NB	�NB	�NB	�NB	�HB	�HB	�NB	�TB	�ZB	�`B	�`B	�fB	�fB	�mB	�fB	�fB	�fB	�mB	�mB	�mB	�mB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
%B
%B
%B
1B
	7B
	7B
	7B

=B
DB
JB
VB
VB
VB
VB
\B
\B
\B
\B
\B
bB
bB
bB
bB
hB
bB
bB
bB
bB
bB
bB
bB
bB
hB
uB
uB
uB
uB
uB
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
#�22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.28 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005190601                              AO  ARCAADJP                                                                    20181005190601    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005190601  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005190601  QCF$                G�O�G�O�G�O�8000            