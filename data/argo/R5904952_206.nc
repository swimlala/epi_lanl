CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:05:52Z creation      
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
resolution        =���   axis      Z        �  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  @$   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  A�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  H�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  J8   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  P�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  W�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  YP   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  `   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  a�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  hh   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  p�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  w�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  y0   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �@   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �D   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �H   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �L   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �P   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20181005190552  20181005190552  5904952 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6431                            2B  A   APEX                            7466                            062512                          846 @���io4
1   @����O��@1�?|�h�c�`A�7L1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �A   A   B   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�33A�33B   B  B��B  B   B(  B0  B8  B@  BG��BP  BX  B`  Bg��Bo��Bx  B�  B�33B�33B�33B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C	�fC  C  C  C  C  C  C�fC  C  C�C   C"  C$  C&  C(  C*  C,  C.  C0�C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  C�  C�  C��C��C��C��C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C��C��C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C��3C��3C�  C��C�  C�  C�  C��3C�  C��C��C��C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C��C��C�  C��3C��3C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3D � D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D	  D	y�D
  D
� D  D� D  D� D  Dy�D  D� D  Dy�D  D� D  D�fD  D� DfD� D  D� D  D� D��D� D  D� D  D� D  D� D  Dy�D  D� D��D� D  D� D  D�fDfD�fD   D � D!fD!�fD"  D"�fD#  D#� D$  D$y�D$��D%y�D%��D2  D2� D2��D3� D3��D4� D5  D5� D5��D6y�D6��D7�fD8  D8� D9  D9� D:  D:� D;  D;� D<fD<� D=  D=y�D=��D>� D?  D?y�D?��D@� DA  DA� DA��DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJfDJ�fDK  DKy�DK��DL� DM  DM� DNfDN�fDO  DO� DP  DP� DQ  DQ� DR  DR�fDS  DS� DS��DT� DU  DU� DV  DV�fDW  DW� DX  DX� DY  DYy�DY��DZy�DZ��D[� D\  D\� D]  D]�fD^  D^� D_fD_�fD`fD`�fDafDa�fDbfDb�fDc  Dc� DdfDd�fDefDe� De��Dfy�Dg  Dg� Dg��Dh� DifDi� Dj  Djy�Dj��Dky�Dk��Dly�Dl��Dmy�Dn  Dn� Do  Do� Dp  Dp� Dq  Dq�fDrfDr�fDs  Ds� Dt  Dt�fDu  Du� Dv  Dv� Dw  Dw� Dw��Dy�RD�1�D�q�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��]@���Az�A$z�ADz�Adz�A�=qA�=qA�=qA�=qA�=qA�=qA�p�A�p�B�B	�B�RB�B!�B)�B1�B9�BA�BH�RBQ�BY�Ba�Bh�RBp�RBy�B��\B�B�B�B��\B�B��\B��\B��\B��\B��\B��\B��\B��\B��\B��\B��\Bď\Bȏ\B̏\B�Bԏ\B؏\B܏\B��\B�\B�\B�\B��\B�\B��\B��\C G�CG�CG�CG�CG�C
.CG�CG�CG�CG�CG�CG�C.CG�CG�CaHC G�C"G�C$G�C&G�C(G�C*G�C,G�C.G�C0aHC2G�C4G�C6G�C8G�C:G�C<G�C>G�C@G�CBG�CDG�CFG�CHG�CJG�CLG�CNG�CPG�CRG�CTG�C�#�C�#�C�0�C�0�C�0�C�0�C�#�C�0�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�
C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�
C�#�C�0�C�0�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�
C�
C�#�C�#�C�
C�
C�#�C�0�C�#�C�#�C�#�C�
C�#�C�0�C�0�C�0�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�
C�
C�#�C�0�C�0�C�#�C�
C�
C�#�C�#�C�#�C�#�C�
C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�
C�
C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�D �D ��D�D��D�D��D�D��D�D�RD�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D�RD�D��DRD��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D�RDRD�RD �D ��D!RD!�RD"�D"�RD#�D#��D$�D$��D%�D%��D&�D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7�RD8�D8��D9�D9��D:�D:��D;�D;��D<RD<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJRDJ�RDK�DK��DL�DL��DM�DM��DNRDN�RDO�DO��DP�DP��DQ�DQ��DR�DR�RDS�DS��DT�DT��DU�DU��DV�DV�RDW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]�RD^�D^��D_RD_�RD`RD`�RDaRDa�RDbRDb�RDc�Dc��DdRDd�RDeRDe��Df�Df��Dg�Dg��Dh�Dh��DiRDi��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq�RDrRDr�RDs�Ds��Dt�Dt�RDu�Du��Dv�Dv��Dw�Dw��Dx�Dy�>D�:�D�z�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�%A�
=A�VA�%A���A���A���A���A��A��A��A��A��A��A��A��A��A��A��A��A���A�  A���A���A���A���A���A���A���A��A��yA��`A��`A���A�ȴA���Aǲ-Aǡ�AǗ�Aǉ7AǃA�~�A�~�Aǝ�AǶFAǩ�Aǝ�Aǉ7A�hsA�\)A�ZA�S�A�/A��A�
=A��AƧ�A�A�A���Aĉ7A��A�7LA���A���A¼jA�dZA�A�`BA��A�%A�oA�jA��A��wA�I�A�;dA��9A��A��TA��`A��hA�1A��PA�O�A��-A�\)A��
A�dZA�%A�  A�x�A��A���A��A�ZA�C�A�33A�
=A�n�A��A���A��A���A�S�A��Ac��A`1'A^�+A\ffA\(�A[�FA[oAZQ�AY��AY�^AX�jAV��AR��AO��AK�AJz�AH�+AGl�AD1'AC�AA�AA�A@��A?x�A>1'A=�A<�9A;�hA8�/A7��A6�yA5
=A1x�A0��A0 �A.jA+��A*ZA)�wA(ȴA&��A$VA"�A" �A!�A��AhsA
=qA��A��A��A&�A�wA��A�\AZA�PAr�A ��@��P@�{@�?}@�bN@��;@�J@��F@�-@�hs@�1@��@�@�  @�"�@�=q@�Ĝ@�z�@�@�(�@�n�@陚@�@�@�ȴ@��@�S�@�{@�hs@�G�@�`B@ᙚ@��@�bN@�9X@߶F@�5?@���@�J@�`B@��/@���@�l�@��y@�x�@؋D@�(�@��
@�l�@؃@� �@�v�@���@�hs@�$�@�n�@ܓu@���@٩�@�J@ج@�t�@��T@�%@�%@�?}@���@�@�J@Ցh@�`B@� �@ҸR@�X@���@�9X@�S�@��@��@���@��@���@��/@̋D@�Z@��;@�S�@ʇ+@��@���@ə�@�?}@�r�@�+@�@�5?@���@�j@�;d@�@�X@��@���@�`B@�dZ@�1'@��
@���@���@�%@�z�@�1'@��m@��@�l�@�+@�@��y@��y@��@��!@��@��#@��@��j@�Z@��
@��F@��P@�@�v�@�V@�{@��T@�@��-@�?}@��@���@��9@��D@�A�@�(�@���@��P@��@���@�V@���@�/@�Z@��;@�|�@�@���@�~�@�5?@��@�@�@��h@�%@��@�Q�@��@��m@��F@���@���@��@�t�@�
=@��!@�n�@�-@���@�O�@�/@�%@���@��@��
@���@��@��P@���@��@�|�@�t�@�K�@�ȴ@�^5@�5?@�$�@���@�?}@���@���@�Ĝ@��D@�b@���@��@��;@���@�ƨ@��w@��P@���@�E�@���@��h@��h@��7@��@�7L@��@���@���@�9X@�A�@�Q�@�bN@�I�@��@�|�@�;d@�33@�+@��@���@��y@��!@�E�@�{@��@��@��@� �@�  @��@���@��@�t�@��H@�ȴ@���@���@�~�@�$�@���@�x�@���@��9@�j@�Q�@�A�@��m@��
@���@��w@���@�dZ@��[@{{J@k��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�%A�
=A�VA�%A���A���A���A���A��A��A��A��A��A��A��A��A��A��A��A��A���A�  A���A���A���A���A���A���A���A��A��yA��`A��`A���A�ȴA���Aǲ-Aǡ�AǗ�Aǉ7AǃA�~�A�~�Aǝ�AǶFAǩ�Aǝ�Aǉ7A�hsA�\)A�ZA�S�A�/A��A�
=A��AƧ�A�A�A���Aĉ7A��A�7LA���A���A¼jA�dZA�A�`BA��A�%A�oA�jA��A��wA�I�A�;dA��9A��A��TA��`A��hA�1A��PA�O�A��-A�\)A��
A�dZA�%A�  A�x�A��A���A��A�ZA�C�A�33A�
=A�n�A��A���A��A���A�S�A��Ac��A`1'A^�+A\ffA\(�A[�FA[oAZQ�AY��AY�^AX�jAV��AR��AO��AK�AJz�AH�+AGl�AD1'AC�AA�AA�A@��A?x�A>1'A=�A<�9A;�hA8�/A7��A6�yA5
=A1x�A0��A0 �A.jA+��A*ZA)�wA(ȴA&��A$VA"�A" �A!�A��AhsA
=qA��A��A��A&�A�wA��A�\AZA�PAr�A ��@��P@�{@�?}@�bN@��;@�J@��F@�-@�hs@�1@��@�@�  @�"�@�=q@�Ĝ@�z�@�@�(�@�n�@陚@�@�@�ȴ@��@�S�@�{@�hs@�G�@�`B@ᙚ@��@�bN@�9X@߶F@�5?@���@�J@�`B@��/@���@�l�@��y@�x�@؋D@�(�@��
@�l�@؃@� �@�v�@���@�hs@�$�@�n�@ܓu@���@٩�@�J@ج@�t�@��T@�%@�%@�?}@���@�@�J@Ցh@�`B@� �@ҸR@�X@���@�9X@�S�@��@��@���@��@���@��/@̋D@�Z@��;@�S�@ʇ+@��@���@ə�@�?}@�r�@�+@�@�5?@���@�j@�;d@�@�X@��@���@�`B@�dZ@�1'@��
@���@���@�%@�z�@�1'@��m@��@�l�@�+@�@��y@��y@��@��!@��@��#@��@��j@�Z@��
@��F@��P@�@�v�@�V@�{@��T@�@��-@�?}@��@���@��9@��D@�A�@�(�@���@��P@��@���@�V@���@�/@�Z@��;@�|�@�@���@�~�@�5?@��@�@�@��h@�%@��@�Q�@��@��m@��F@���@���@��@�t�@�
=@��!@�n�@�-@���@�O�@�/@�%@���@��@��
@���@��@��P@���@��@�|�@�t�@�K�@�ȴ@�^5@�5?@�$�@���@�?}@���@���@�Ĝ@��D@�b@���@��@��;@���@�ƨ@��w@��P@���@�E�@���@��h@��h@��7@��@�7L@��@���@���@�9X@�A�@�Q�@�bN@�I�@��@�|�@�;d@�33@�+@��@���@��y@��!@�E�@�{@��@��@��@� �@�  @��@���@��@�t�@��H@�ȴ@���@���@�~�@�$�@���@�x�@���@��9@�j@�Q�@�A�@��m@��
@���@��w@���@�dZ@��[@{{J@k��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B%B%B%B%BBBBBBBBBBBBBBBBBBBBBBBBBBBBBB
��B
��B
��B
��B
��B
��B
��B
��B
��B
��BB
=B�B�B{B{BuB�B&�B=qBL�B^5Bl�BjB�DB��B�!B�dB�9B�XB�qB�}BǮB��B�B�BB�fB�B��BB
=BoBhB�B.B33B;dBF�BF�BF�BE�BC�BE�BD�BD�BD�BB�B;dB.B �B�BB�B�yB�/B��B�RB��B~�Bw�Br�Bm�B	T�B	>wB	33B	&�B	%�B	#�B	�B	�B	�B	�B	�B	\B��B�B�B�B�B�ZB�B�B��B��B��B��BǮBŢBB�wB�}B�jB�dB�^B�qB�dB�XB�LB�LB�9B�!B�B��B��B�hB�VA�oB�wB�}B�dB�dB�^B�dB�^B�^B�^B�wBÖBĜBBĜBŢBƨBƨBƨBĜB��B��B��B��BÖBĜBƨB��B��B��B��B��B�B�)B�#B�B�B�B�)B�/B�HB�NB�`B�sB�sB�B�B�B�B�B�B��B��B��B��B��B��B	  B	B	+B	1B	JB	bB	�B	�B	8RB	@�B	E�B	e`B	l�B	jB	o�B	l�B	v�B	t�B	s�B	s�B	w�B	y�B	~�B	�B	�%B	�1B	�+B	�1B	�7B	�JB	�PB	�JB	�PB	�\B	�\B	�VB	�\B	�bB	�bB	�bB	�hB	�hB	�hB	�oB	�oB	�oB	�oB	�oB	�oB	�oB	�hB	�PB	�oB	�PB	�PB	�PB	�JB	�7B	�%B	�1B	�B	{�B	�B	�B	�+B	�%B	�%B	�B	�+B	�7B	�PB	�PB	�\B	�\B	�\B	�bB	�hB	�hB	�VB	�LB	�FB	�RB	�XB	�XB	�dB	�jB	�jB	�qB	��B	ÖB	ĜB	ŢB	ŢB	ƨB	ǮB	ǮB	ȴB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�
B	�
B	�B	�B	�B	�/B	�BB	�BB	�HB	�NB	�TB	�TB	�TB	�TB	�TB	�ZB	�fB	�mB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B	��B
B
B
B
B
B
B
B
B
B
B
%B
%B
+B
1B
1B
1B
1B
1B
	7B

=B
DB
DB
DB
DB
JB
JB
JB
PB
VB
VB
\B
VB
\B
\B
\B
\B
bB
bB
B
#�B
0222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222224222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222   B%B%B%B%BBBBBBBBBBBBBBBBBBBBBBBBBBBBBB
��B
��B
��B
��B
��B
��B
��B
��B
��B
��BB
=B�B�B{B{BuB�B&�B=qBL�B^5Bl�BjB�DB��B�!B�dB�9B�XB�qB�}BǮB��B�B�BB�fB�B��BB
=BoBhB�B.B33B;dBF�BF�BF�BE�BC�BE�BD�BD�BD�BB�B;dB.B �B�BB�B�yB�/B��B�RB��B~�Bw�Br�Bm�B	T�B	>wB	33B	&�B	%�B	#�B	�B	�B	�B	�B	�B	\B��B�B�B�B�B�ZB�B�B��B��B��B��BǮBŢBB�wB�}B�jB�dB�^B�qB�dB�XB�LB�LB�9B�!B�B��B��B�hB�VA�oB�wB�}B�dB�dB�^B�dB�^B�^B�^B�wBÖBĜBBĜBŢBƨBƨBƨBĜB��B��B��B��BÖBĜBƨB��B��B��B��B��B�B�)B�#B�B�B�B�)B�/B�HB�NB�`B�sB�sB�B�B�B�B�B�B��B��B��B��B��B��B	  B	B	+B	1B	JB	bB	�B	�B	8RB	@�B	E�B	e`B	l�B	jB	o�B	l�B	v�B	t�B	s�B	s�B	w�B	y�B	~�B	�B	�%B	�1B	�+B	�1B	�7B	�JB	�PB	�JB	�PB	�\B	�\B	�VB	�\B	�bB	�bB	�bB	�hB	�hB	�hB	�oB	�oB	�oB	�oB	�oB	�oB	�oB	�hB	�PB	�oB	�PB	�PB	�PB	�JB	�7B	�%B	�1B	�B	{�B	�B	�B	�+B	�%B	�%B	�B	�+B	�7B	�PB	�PB	�\B	�\B	�\B	�bB	�hB	�hB	�VB	�LB	�FB	�RB	�XB	�XB	�dB	�jB	�jB	�qB	��B	ÖB	ĜB	ŢB	ŢB	ƨB	ǮB	ǮB	ȴB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�
B	�
B	�B	�B	�B	�/B	�BB	�BB	�HB	�NB	�TB	�TB	�TB	�TB	�TB	�ZB	�fB	�mB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B	��B
B
B
B
B
B
B
B
B
B
B
%B
%B
+B
1B
1B
1B
1B
1B
	7B

=B
DB
DB
DB
DB
JB
JB
JB
PB
VB
VB
\B
VB
\B
\B
\B
\B
bB
bB
B
#�B
0222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222224222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.28 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005190552                              AO  ARCAADJP                                                                    20181005190552    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005190552  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005190552  QCF$                G�O�G�O�G�O�8000            