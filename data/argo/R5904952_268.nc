CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:06:06Z creation      
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20181005190606  20181005190606  5904952 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL              A   AO  6431                            2B  A   APEX                            7466                            062512                          846 @��i�~��1   @��j33H@22n��O��c��`A�71   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                     A   A   A   @�33@�  A   A   A@  A`  A�  A�33A�  A�  A�  A�  A�  A�33B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bg��Bp  BxffB�  B�  B�  B�  B�33B�  B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C �C�C�C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C7�fC:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Ce�fCh  Cj�Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz�C|  C~  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C��C�  C�  C��3C�  C�  C�  C�  C�  C��C�  C�  C��3C��3C�  C��3C��3C��3C��3C�  C��C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C��3C�  C��C��C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C��C��C��C�  C�  C�  C�  C�  C��C�  C�  C�  D   D � D  Dy�D  D� D��Dy�D  D� D  D� D  D� D��D� D  D� D	  D	� D	��D
y�D  D� D��Dy�D  D� D  D�fDfD�fD  Dy�D��D� D  D� D  D� DfD� D��D� D  D� D  D� D  D� D  D� D��D�fD  Dy�D��D� DfD� D��D� D  D� D��D � D!  D!� D"  D"� D#  D#� D#��D$� D%  D%� D&  D&� D&��D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D-��D.� D/  D/� D0fD0� D1  D1� D2  D2� D3  D3y�D3��D4� D5  D5� D5��D6� D7fD7� D7��D8� D9  D9� D9��D:y�D;  D;� D<  D<� D=  D=� D>  D>y�D?  D?� D@  D@� DA  DA� DB  DB�fDC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DHfDH� DH��DI� DJ  DJ� DK  DK� DL  DL� DL��DM� DN  DN� DO  DO�fDP  DPy�DP��DQ� DR  DR� DS  DS� DT  DT� DU  DU� DU��DVy�DW  DW� DW��DXy�DX��DYy�DZ  DZ�fD[fD[�fD\fD\�fD]fD]� D^  D^� D_  D_� D`fD`�fDa  Da� DbfDb�fDc  Dcy�Dc��Dd� De  De� Df  Df� Df��Dgy�DhfDh�fDifDi� Di��Dj� Dk  Dk� Dl  Dl�fDmfDm� Dn  Dn� Do  Do�fDp  Dp� Dq  Dq� DrfDr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dwy�Dw��Dy��D�!HD���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�(�@���Az�A$z�ADz�Adz�A�=qA�p�A�=qA�=qA�=qA�=qA�=qA�p�B�B	�B�B�B!�B)�B1�B9�BA�BI�BQ�BY�Ba�Bh�RBq�By�B��\B��\B��\B��\B�B��\B�\)B�\)B��\B��\B��\B��\B��\B��\B��\B��\B��\B�\)B�\)B̏\BЏ\Bԏ\B؏\B܏\B��\B�\B�\B�\B��\B�\B��\B��\C aHCaHCaHCG�CG�C
G�CG�CG�CG�CG�CG�CG�CG�CG�CG�CG�C G�C"G�C$G�C&G�C(G�C*G�C,G�C.G�C0G�C2G�C4G�C6G�C8.C:G�C<G�C>G�C@G�CBG�CDG�CFG�CHG�CJG�CLG�CNG�CPG�CRG�CTG�CVG�CXG�CZG�C\G�C^G�C`G�CbG�CdG�Cf.ChG�CjaHClG�CnG�CpG�CrG�CtG�CvG�CxG�CzaHC|G�C~G�C�#�C�#�C�#�C�#�C�#�C�#�C�0�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�
C�
C�#�C�#�C�#�C�#�C�#�C�0�C�#�C�#�C�
C�#�C�#�C�#�C�#�C�#�C�0�C�#�C�#�C�
C�
C�#�C�
C�
C�
C�
C�#�C�0�C�#�C�0�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�
C�#�C�#�C�#�C�#�C�#�C�
C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�
C�#�C�#�C�#�C�
C�
C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�
C�
C�
C�#�C�0�C�0�C�#�C�#�C�0�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�
C�
C�#�C�0�C�0�C�0�C�#�C�#�C�#�C�#�C�#�C�0�C�#�C�#�C�#�D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D�RDRD�RD�D��D�D��D�D��D�D��DRD��D�D��D�D��D�D��D�D��D�D��D�D�RD�D��D�D��DRD��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0RD0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7RD7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB�RDC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DHRDH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO�RDP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ�RD[RD[�RD\RD\�RD]RD]��D^�D^��D_�D_��D`RD`�RDa�Da��DbRDb�RDc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��DhRDh�RDiRDi��Dj�Dj��Dk�Dk��Dl�Dl�RDmRDm��Dn�Dn��Do�Do�RDp�Dp��Dq�Dq��DrRDr��Ds�Ds��Dt�Dt��Du�Du��Dv�Dv��Dw�Dw��Dw޹Dy�{D�*>D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��A��A�"�A�"�A��A��A��A� �A� �A�"�A�"�A�"�A� �A�"�A�$�A�$�A�"�A�"�A�$�A�$�A�"�A�$�A�&�A�&�A�&�A�&�A�-A�-A�-A�-A�/A�/A�1'A�33A�5?A�5?A�5?A�5?A�5?A�5?A�5?A�5?A�33A�"�Aȕ�A��A��AŁAăA�A���A��mA�A�A�C�A��A���A��A�I�A��DA�r�A� �A���A�A��yA��jA�A�A��PA�33A��TA�bNA��hA�^5A�z�A��DA�1'A��/A���A�S�A��FA��mA���A�9XA�9XA�;dA�E�A��A���A�ffA�n�A��A�5?A�bA���A���A�
=A��A�1'A���A�hsA���A~{AzAx�Au�^Aq&�An�Am�#AlVAj �Ae/Aa�A_oA]hsA[�AZ5?AYG�AX�/AV��AQ�AQp�AP�/AO��AM��AK�mAJ-AH�/AGVAF  ADr�AA��A@VA@  A?��A>�RA=/A;33A7�A5t�A5oA4�RA4E�A3�A2�RA0r�A-��A-�7A,�/A,�A+�TA+7LA*I�A)�A'p�A&�+A%��A%O�A$�A$jA#�
A#��A#\)A#VA"�A"r�A"-A!�#A!p�A!�A!�A �/A��A&�AjA33A�DA�FA��A5?A��A%A�AO�A9XAS�Ar�AA+A�AĜA��AbNAK�A�A5?A�At�A
Q�A
JA	A5?A��A"�A�uAM�AA�A��A�HAn�A(�A�FA��AC�A�A�TA%A ��A J@�"�@��@��@�C�@��@�E�@�ƨ@�n�@��+@��u@��@�J@���@�x�@�%@�@�"�@��@��@�@�1'@��;@�dZ@���@�E�@�~�@�5?@�V@�j@�bN@�I�@�A�@��T@�~�@�ƨ@߶F@�1@�Q�@���@�/@�p�@�O�@��@�  @�;d@ޏ\@�^5@�{@��@��@���@݁@ܴ9@�A�@�1@۝�@�\)@�@��@�n�@�X@��@ج@�1@�t�@���@�$�@�@�X@�/@Դ9@� �@��
@ӕ�@ҏ\@�{@ёh@��/@�Z@϶F@�t�@�\)@�\)@�C�@��#@�G�@̼j@̃@�bN@�(�@���@�p�@�{@���@��T@ͺ^@͑h@́@�&�@���@̓u@�r�@��@���@��`@�r�@�I�@�A�@ǶF@ǍP@ǅ@�l�@�C�@�"�@��H@�E�@ź^@�x�@���@�  @�33@�~�@�V@�=q@���@��@�j@� �@�dZ@��@�@���@�@��9@�9X@��
@�l�@�C�@���@��y@��@��R@�5?@���@�`B@���@� �@�l�@�n�@�`B@�G�@�X@�?}@�Z@���@�E�@��\@�dZ@��H@�E�@��h@��j@��@��@�\)@�@��R@�M�@�@�J@��#@���@���@���@���@��u@�l�@��R@�J@��#@��#@���@��7@��@��`@�A�@��P@�l�@�;d@�"�@���@���@���@�^5@�J@�?}@��`@��9@�Q�@�b@��m@��m@��
@���@��w@�\)@�;d@�
=@�-@�@�X@�?}@�/@���@���@�bN@��@��;@��;@��H@�^5@�=q@�-@�@��-@�X@���@��D@�Z@�I�@���@��w@���@�+@��@���@�~�@�ff@�ff@�E�@�@��h@���@��@�j@�I�@�1'@��m@�l�@�+@��H@��R@���@�n�@�{@��@��T@��@��@���@�r�@�Q�@�(�@�  @���@�+@��@���@�^5@�V@�-@��@���@�hs@�G�@��@���@��D@��.@�^�@yB�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��A��A�"�A�"�A��A��A��A� �A� �A�"�A�"�A�"�A� �A�"�A�$�A�$�A�"�A�"�A�$�A�$�A�"�A�$�A�&�A�&�A�&�A�&�A�-A�-A�-A�-A�/A�/A�1'A�33A�5?A�5?A�5?A�5?A�5?A�5?A�5?A�5?A�33A�"�Aȕ�A��A��AŁAăA�A���A��mA�A�A�C�A��A���A��A�I�A��DA�r�A� �A���A�A��yA��jA�A�A��PA�33A��TA�bNA��hA�^5A�z�A��DA�1'A��/A���A�S�A��FA��mA���A�9XA�9XA�;dA�E�A��A���A�ffA�n�A��A�5?A�bA���A���A�
=A��A�1'A���A�hsA���A~{AzAx�Au�^Aq&�An�Am�#AlVAj �Ae/Aa�A_oA]hsA[�AZ5?AYG�AX�/AV��AQ�AQp�AP�/AO��AM��AK�mAJ-AH�/AGVAF  ADr�AA��A@VA@  A?��A>�RA=/A;33A7�A5t�A5oA4�RA4E�A3�A2�RA0r�A-��A-�7A,�/A,�A+�TA+7LA*I�A)�A'p�A&�+A%��A%O�A$�A$jA#�
A#��A#\)A#VA"�A"r�A"-A!�#A!p�A!�A!�A �/A��A&�AjA33A�DA�FA��A5?A��A%A�AO�A9XAS�Ar�AA+A�AĜA��AbNAK�A�A5?A�At�A
Q�A
JA	A5?A��A"�A�uAM�AA�A��A�HAn�A(�A�FA��AC�A�A�TA%A ��A J@�"�@��@��@�C�@��@�E�@�ƨ@�n�@��+@��u@��@�J@���@�x�@�%@�@�"�@��@��@�@�1'@��;@�dZ@���@�E�@�~�@�5?@�V@�j@�bN@�I�@�A�@��T@�~�@�ƨ@߶F@�1@�Q�@���@�/@�p�@�O�@��@�  @�;d@ޏ\@�^5@�{@��@��@���@݁@ܴ9@�A�@�1@۝�@�\)@�@��@�n�@�X@��@ج@�1@�t�@���@�$�@�@�X@�/@Դ9@� �@��
@ӕ�@ҏ\@�{@ёh@��/@�Z@϶F@�t�@�\)@�\)@�C�@��#@�G�@̼j@̃@�bN@�(�@���@�p�@�{@���@��T@ͺ^@͑h@́@�&�@���@̓u@�r�@��@���@��`@�r�@�I�@�A�@ǶF@ǍP@ǅ@�l�@�C�@�"�@��H@�E�@ź^@�x�@���@�  @�33@�~�@�V@�=q@���@��@�j@� �@�dZ@��@�@���@�@��9@�9X@��
@�l�@�C�@���@��y@��@��R@�5?@���@�`B@���@� �@�l�@�n�@�`B@�G�@�X@�?}@�Z@���@�E�@��\@�dZ@��H@�E�@��h@��j@��@��@�\)@�@��R@�M�@�@�J@��#@���@���@���@���@��u@�l�@��R@�J@��#@��#@���@��7@��@��`@�A�@��P@�l�@�;d@�"�@���@���@���@�^5@�J@�?}@��`@��9@�Q�@�b@��m@��m@��
@���@��w@�\)@�;d@�
=@�-@�@�X@�?}@�/@���@���@�bN@��@��;@��;@��H@�^5@�=q@�-@�@��-@�X@���@��D@�Z@�I�@���@��w@���@�+@��@���@�~�@�ff@�ff@�E�@�@��h@���@��@�j@�I�@�1'@��m@�l�@�+@��H@��R@���@�n�@�{@��@��T@��@��@���@�r�@�Q�@�(�@�  @���@�+@��@���@�^5@�V@�-@��@���@�hs@�G�@��@���@��D@��.@�^�@yB�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BK�BK�BL�BL�BK�BK�BK�BK�BK�BK�BK�BK�BK�BK�BK�BJ�BJ�BK�BK�BK�BK�BK�BK�BK�BK�BK�BK�BK�BK�BK�BK�BK�BK�BK�BK�BK�BK�BK�BK�BK�BK�BK�BK�BK�B��B��B	�B	:^B	r�B	�`B
2-B
��B
��B
�-B
�HB
�BB%�BJ�B-B33B[#Be`B}�B~�B`BB7LB,B"�B�B7LB(�B�B�B+B
�TB
�B
��B
��B
ŢB
�B
��B
��B
�3B
�B
�B
�B
��B
ĜB
�9B
��B
~�B
J�B
7LB
.B
$�B
bB	��B	ÖB	��B	�DB	�B	}�B	|�B	n�B	aHB	_;B	W
B	H�B	1'B	�B	hB		7B	B��B��B�B�mB�TB�`B�`B�TB�ZB�NB�HB�;B�B��B��B��B��B��B��B��B��BŢB�qB�^B�XB�RB�LB�FB�9B�9B�9B�-B�-B�'B�B�B�B�B�B�B�B�B�B�'B�?B�LB�^B�dB�jB�qB�jB�^B�^B��B��B��B��B��B��B��B�
B�B�/B�`B�yB�B��B	B	  B	  B��B��B	  B	B��B��B��B��B��B��B��B	B	B		7B	DB	JB	PB	uB	uB	�B	�B	A�B	F�B	D�B	A�B	=qB	7LB	2-B	0!B	2-B	0!B	1'B	6FB	7LB	:^B	=qB	A�B	T�B	T�B	M�B	P�B	^5B	aHB	ffB	gmB	ffB	ffB	ffB	gmB	ffB	e`B	dZB	dZB	e`B	m�B	l�B	hsB	aHB	T�B	F�B	D�B	G�B	I�B	K�B	M�B	aHB	l�B	iyB	p�B	t�B	y�B	�B	�+B	�JB	�PB	�\B	�bB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�'B	�3B	�?B	�LB	�RB	�^B	�^B	�jB	�qB	�qB	�qB	�wB	�}B	�}B	�}B	��B	��B	��B	��B	��B	�}B	��B	��B	B	ÖB	ÖB	ĜB	��B	��B	�B	�#B	�#B	�#B	�#B	�#B	�#B	�#B	�#B	�B	�B	��B	��B	��B	��B	��B	��B	�B	�)B	�)B	�)B	�/B	�HB	�TB	�`B	�`B	�`B	�ZB	�fB	�`B	�`B	�`B	�`B	�`B	�ZB	�ZB	�`B	�sB	�yB	�sB	�yB	�B	�B	�yB	�B	�yB	�yB	�B	�B	�B	�B	�B	�B	�yB	�fB	�TB	�TB	�TB	�TB	�TB	�TB	�TB	�ZB	�TB	�yB	�B	�B	�B	�B	�B	�B	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
+B
1B
+B
	7B
	7B
	7B
	7B
	7B
	7B

=B

=B
DB
DB
DB
DB
JB
JB
JB
JB
DB

=B
1B
1B
1B
1B
DB

=B
	7B
	7B
	7B
1B
1B
1B
1B
1B
	7B

=B
DB
DB
bB
�B
�B
&L222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  BK�BK�BL�BL�BK�BK�BK�BK�BK�BK�BK�BK�BK�BK�BK�BJ�BJ�BK�BK�BK�BK�BK�BK�BK�BK�BK�BK�BK�BK�BK�BK�BK�BK�BK�BK�BK�BK�BK�BK�BK�BK�BK�BK�BK�B��B��B	�B	:^B	r�B	�`B
2-B
��B
��B
�-B
�HB
�BB%�BJ�B-B33B[#Be`B}�B~�B`BB7LB,B"�B�B7LB(�B�B�B+B
�TB
�B
��B
��B
ŢB
�B
��B
��B
�3B
�B
�B
�B
��B
ĜB
�9B
��B
~�B
J�B
7LB
.B
$�B
bB	��B	ÖB	��B	�DB	�B	}�B	|�B	n�B	aHB	_;B	W
B	H�B	1'B	�B	hB		7B	B��B��B�B�mB�TB�`B�`B�TB�ZB�NB�HB�;B�B��B��B��B��B��B��B��B��BŢB�qB�^B�XB�RB�LB�FB�9B�9B�9B�-B�-B�'B�B�B�B�B�B�B�B�B�B�'B�?B�LB�^B�dB�jB�qB�jB�^B�^B��B��B��B��B��B��B��B�
B�B�/B�`B�yB�B��B	B	  B	  B��B��B	  B	B��B��B��B��B��B��B��B	B	B		7B	DB	JB	PB	uB	uB	�B	�B	A�B	F�B	D�B	A�B	=qB	7LB	2-B	0!B	2-B	0!B	1'B	6FB	7LB	:^B	=qB	A�B	T�B	T�B	M�B	P�B	^5B	aHB	ffB	gmB	ffB	ffB	ffB	gmB	ffB	e`B	dZB	dZB	e`B	m�B	l�B	hsB	aHB	T�B	F�B	D�B	G�B	I�B	K�B	M�B	aHB	l�B	iyB	p�B	t�B	y�B	�B	�+B	�JB	�PB	�\B	�bB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�'B	�3B	�?B	�LB	�RB	�^B	�^B	�jB	�qB	�qB	�qB	�wB	�}B	�}B	�}B	��B	��B	��B	��B	��B	�}B	��B	��B	B	ÖB	ÖB	ĜB	��B	��B	�B	�#B	�#B	�#B	�#B	�#B	�#B	�#B	�#B	�B	�B	��B	��B	��B	��B	��B	��B	�B	�)B	�)B	�)B	�/B	�HB	�TB	�`B	�`B	�`B	�ZB	�fB	�`B	�`B	�`B	�`B	�`B	�ZB	�ZB	�`B	�sB	�yB	�sB	�yB	�B	�B	�yB	�B	�yB	�yB	�B	�B	�B	�B	�B	�B	�yB	�fB	�TB	�TB	�TB	�TB	�TB	�TB	�TB	�ZB	�TB	�yB	�B	�B	�B	�B	�B	�B	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
+B
1B
+B
	7B
	7B
	7B
	7B
	7B
	7B

=B

=B
DB
DB
DB
DB
JB
JB
JB
JB
DB

=B
1B
1B
1B
1B
DB

=B
	7B
	7B
	7B
1B
1B
1B
1B
1B
	7B

=B
DB
DB
bB
�B
�B
&L222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.28 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005190606                              AO  ARCAADJP                                                                    20181005190606    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005190606  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005190606  QCF$                G�O�G�O�G�O�8000            