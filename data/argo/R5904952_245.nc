CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:06:00Z creation      
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
_FillValue                    �0Argo profile    3.1 1.2 19500101000000  20181005190600  20181005190600  5904952 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6431                            2B  A   APEX                            7466                            062512                          846 @���"1   @������ @1I�^�c����S�1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �A   A   A   @9��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�33A�  B   B  B  B  B   B(  B0  B8  B@  BG��BO��BW��B`  Bh  Bp  Bx  B�  B���B�  B�33B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C-�fC0  C2  C4  C6�C8�C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CW�fCZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj�Cl  Cn�Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C��C��C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C��C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C��C�  C��3C�  C��C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C��C�  D   D � D  D� D  D� D  D� D  Dy�D  D� D  Dy�D��Dy�D��Dy�D��D	y�D	��D
� D  D� D  D�fD  D� D  D� D  D� D  D� D  Dy�D��Dy�D��D�fD  Dy�D  D� D  Dy�D  D� D��D� DfD�fDfD� D  D� D  D� D  D� D��D� D  D� D   D � D!  D!y�D!��D"� D#fD#�fD$fD$�fD%  D%� D&  D&� D'  D'�fD(fD(y�D(��D)� D*  D*y�D*��D+� D,  D,� D-  D-� D.  D.�fD/  D/y�D0  D0� D1  D1y�D1��D2y�D2��D3� D4  D4� D5fD5�fD6fD6�fD7fD7� D8  D8y�D9  D9� D:  D:�fD;  D;� D<  D<� D=fD=� D>  D>� D?  D?� D?��D@y�DA  DA� DB  DBy�DC  DC� DD  DD� DE  DE� DF  DF� DG  DG�fDH  DH� DI  DI� DI��DJ� DK  DKy�DK��DL� DMfDM� DN  DN� DO  DOy�DP  DPy�DQ  DQ� DR  DRy�DS  DS� DT  DTy�DU  DU�fDVfDV�fDW  DWy�DX  DX�fDY  DY� DZ  DZ� D[fD[�fD\fD\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Da��Db� Dc  Dc� Dd  Dd� DefDe� Df  Df� Dg  Dg� Dh  Dhy�Dh��Diy�Dj  Dj�fDkfDk� Dl  Dl� Dm  Dm� Dn  Dn�fDofDo�fDpfDp�fDqfDq�fDr  Dry�Ds  Ds�fDtfDt�fDu  Duy�Du��Dvy�Dw  Dw� Dw�3Dy�D�K�D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @K�@���@���Az�A$z�ADz�Adz�A�=qA�=qA�=qA�=qA�=qA�=qA�p�A�=qB�B	�B�B�B!�B)�B1�B9�BA�BH�RBP�RBX�RBa�Bi�Bq�By�B��\B�\)B��\B�B��\B��\B��\B�B��\B��\B��\B��\B��\B��\B��\B��\B��\Bď\B�B̏\BЏ\Bԏ\B؏\B܏\B��\B�\B�\B�\B��\B�\B��\B��\C G�CG�CG�CG�CG�C
G�CG�CG�CG�CG�CG�CG�CG�CG�CG�CG�C G�C"G�C$G�C&G�C(G�C*G�C,G�C..C0G�C2G�C4G�C6aHC8aHC:G�C<G�C>G�C@G�CBG�CDG�CFG�CHG�CJG�CLG�CNG�CPG�CRG�CTG�CVG�CX.CZG�C\G�C^G�C`G�CbG�CdG�CfG�ChG�CjaHClG�CnaHCpG�CrG�CtG�CvG�CxG�CzG�C|G�C~G�C�#�C�#�C�#�C�#�C�#�C�
C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�
C�#�C�#�C�0�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�
C�
C�#�C�0�C�0�C�#�C�#�C�0�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�0�C�#�C�#�C�#�C�#�C�#�C�#�C�
C�#�C�#�C�#�C�#�C�#�C�#�C�
C�
C�#�C�#�C�#�C�0�C�#�C�
C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�
C�#�C�0�C�#�C�
C�#�C�0�C�#�C�#�C�#�C�#�C�#�C�#�C�
C�
C�#�C�#�C�#�C�0�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�
C�#�C�0�C�#�D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D�RD�D��D�D��D�D��D�D��D�D��D�D��D�D�RD�D��D�D��D�D��D�D��D�D��DRD�RDRD��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#RD#�RD$RD$�RD%�D%��D&�D&��D'�D'�RD(RD(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.�RD/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5RD5�RD6RD6�RD7RD7��D8�D8��D9�D9��D:�D:�RD;�D;��D<�D<��D=RD=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG�RDH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DMRDM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU�RDVRDV�RDW�DW��DX�DX�RDY�DY��DZ�DZ��D[RD[�RD\RD\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��DeRDe��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj�RDkRDk��Dl�Dl��Dm�Dm��Dn�Dn�RDoRDo�RDpRDp�RDqRDq�RDr�Dr��Ds�Ds�RDtRDt�RDu�Du��Dv�Dv��Dw�Dw��DxDy� D�T{D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A˸RA˺^A˼jA˺^A˸RAˬAˣ�Aˇ+Aʩ�AȸRA�ƨA�VA� �A���A��A���AǓuA�hsA�I�A�(�A��A��Aƺ^AƁA�S�A�E�A�A�A�33A�33A�dZA���AƑhA�l�A�bNA�C�A��A��`Aź^Aś�A�O�A���A�A�%A��`Ać+A��`A�\)A�A�l�A�bNA��A�-A���A��/A�%A�XA��/A��HA�Q�A�1A�;dA���A��A�p�A�&�A��9A�;dA�bA��PA��hA�ȴA�x�A�1A���A�bNA�1'A�M�A�hsA�ZA��A��yA���A�ĜA�M�A�oA���A�Q�A���A���A��yA�\)A�t�A��!A�hsA���A���A���A�-AwhsAr��AoXAm+Ai�Ah�Ahv�Ag?}Aex�Ac�
A_�A]+A[��AZAX�AXJAW��AVĜAR�AQ��AQ�AP�DAKƨAH��AI�AHz�AF�AF1AD�AC`BA@��A<�A:�A9��A8��A4bNA/"�A-�TA-A+�A(n�A&�9A%�TA%hsA$�`A$��A$I�A"�/A �A��AG�A?}A&�A
=A�AĜA�wAn�AI�A�A
=AI�A�9AA"�A �AVA��A  A�A�A��Az�A�AE�A��A��A\)A"�A��A�/A��AQ�A�#AXA
��A
n�A	A	x�A	S�A��A�A��AK�A��A�AVA�yAE�A�TA&�A r�@��+@�O�@���@��T@�hs@�@�ff@�X@�I�@�!@���@�9X@��;@�;d@�n�@�7L@�D@��@�n�@���@�O�@��/@��
@��@�9@߮@ߍP@��@�$�@�G�@��@݁@�r�@ج@׍P@�o@�n�@�/@�ƨ@ӕ�@�;d@��y@Ұ!@�=q@ёh@�O�@�%@д9@�(�@ϕ�@�+@�
=@�v�@��@��#@�x�@��@̴9@�;d@ʸR@�E�@�@�hs@�/@���@���@�Q�@�E�@�O�@�7L@ě�@�K�@+@��@�X@���@�r�@��@�;d@��H@���@���@�E�@���@���@�x�@�?}@��9@�bN@�b@���@���@�t�@�5?@��T@�@�G�@��@���@��/@���@��@���@��D@��D@��u@��D@�Q�@��9@��D@�1'@�b@��P@��@���@�v�@�V@��#@��@�7L@���@���@���@�bN@�Q�@�A�@��@�\)@�~�@�x�@�&�@���@��/@��@�G�@��-@�/@��@�(�@�\)@��H@��@���@���@��!@���@�ȴ@��!@�-@��-@���@�x�@�?}@���@���@�r�@�Q�@���@���@��@�dZ@�33@��@���@�n�@���@��T@��^@�p�@�G�@���@�z�@�1'@�  @��@�"�@�o@��y@��R@�~�@�E�@��^@�O�@��`@��j@��u@��@��m@��w@�dZ@�@���@�@�o@���@��@��@��@�O�@���@�%@�V@�/@��`@��@��
@��m@���@�dZ@�"�@�
=@�@���@���@�@��h@�7L@�&�@��@�A�@�b@�  @���@��w@���@�dZ@�;d@�"�@��\@��@��@��@�7L@�%@��@�I�@�b@��@��F@�33@��@�@��!@���@��\@�=q@���@���@���@�hs@�/@��@���@�A�@���@�\)@�;d@�
=@��H@���@�M�@�-@�J@���@�@�G�@��j@��@��@��@��@�o@��H@��R@���@�V@�5?@�@��@�J@�{@��@�=q@�@���@�?}@��u@���@���@��@�j@�Q�@�1'@�1@��@���@��,@O@i�71111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A˸RA˺^A˼jA˺^A˸RAˬAˣ�Aˇ+Aʩ�AȸRA�ƨA�VA� �A���A��A���AǓuA�hsA�I�A�(�A��A��Aƺ^AƁA�S�A�E�A�A�A�33A�33A�dZA���AƑhA�l�A�bNA�C�A��A��`Aź^Aś�A�O�A���A�A�%A��`Ać+A��`A�\)A�A�l�A�bNA��A�-A���A��/A�%A�XA��/A��HA�Q�A�1A�;dA���A��A�p�A�&�A��9A�;dA�bA��PA��hA�ȴA�x�A�1A���A�bNA�1'A�M�A�hsA�ZA��A��yA���A�ĜA�M�A�oA���A�Q�A���A���A��yA�\)A�t�A��!A�hsA���A���A���A�-AwhsAr��AoXAm+Ai�Ah�Ahv�Ag?}Aex�Ac�
A_�A]+A[��AZAX�AXJAW��AVĜAR�AQ��AQ�AP�DAKƨAH��AI�AHz�AF�AF1AD�AC`BA@��A<�A:�A9��A8��A4bNA/"�A-�TA-A+�A(n�A&�9A%�TA%hsA$�`A$��A$I�A"�/A �A��AG�A?}A&�A
=A�AĜA�wAn�AI�A�A
=AI�A�9AA"�A �AVA��A  A�A�A��Az�A�AE�A��A��A\)A"�A��A�/A��AQ�A�#AXA
��A
n�A	A	x�A	S�A��A�A��AK�A��A�AVA�yAE�A�TA&�A r�@��+@�O�@���@��T@�hs@�@�ff@�X@�I�@�!@���@�9X@��;@�;d@�n�@�7L@�D@��@�n�@���@�O�@��/@��
@��@�9@߮@ߍP@��@�$�@�G�@��@݁@�r�@ج@׍P@�o@�n�@�/@�ƨ@ӕ�@�;d@��y@Ұ!@�=q@ёh@�O�@�%@д9@�(�@ϕ�@�+@�
=@�v�@��@��#@�x�@��@̴9@�;d@ʸR@�E�@�@�hs@�/@���@���@�Q�@�E�@�O�@�7L@ě�@�K�@+@��@�X@���@�r�@��@�;d@��H@���@���@�E�@���@���@�x�@�?}@��9@�bN@�b@���@���@�t�@�5?@��T@�@�G�@��@���@��/@���@��@���@��D@��D@��u@��D@�Q�@��9@��D@�1'@�b@��P@��@���@�v�@�V@��#@��@�7L@���@���@���@�bN@�Q�@�A�@��@�\)@�~�@�x�@�&�@���@��/@��@�G�@��-@�/@��@�(�@�\)@��H@��@���@���@��!@���@�ȴ@��!@�-@��-@���@�x�@�?}@���@���@�r�@�Q�@���@���@��@�dZ@�33@��@���@�n�@���@��T@��^@�p�@�G�@���@�z�@�1'@�  @��@�"�@�o@��y@��R@�~�@�E�@��^@�O�@��`@��j@��u@��@��m@��w@�dZ@�@���@�@�o@���@��@��@��@�O�@���@�%@�V@�/@��`@��@��
@��m@���@�dZ@�"�@�
=@�@���@���@�@��h@�7L@�&�@��@�A�@�b@�  @���@��w@���@�dZ@�;d@�"�@��\@��@��@��@�7L@�%@��@�I�@�b@��@��F@�33@��@�@��!@���@��\@�=q@���@���@���@�hs@�/@��@���@�A�@���@�\)@�;d@�
=@��H@���@�M�@�-@�J@���@�@�G�@��j@��@��@��@��@�o@��H@��R@���@�V@�5?@�@��@�J@�{@��@�=q@�@���@�?}@��u@���@���@��@�j@�Q�@�1'@�1@��@���@��,@O@i�71111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BBBBBBBB  BB#�BD�Bq�Bu�B~�B�7B��B�9B��B��B�5B�`B�B��B��B	B	JB	�B	�B	'�B	=qB	aHB	cTB	hsB	p�B	s�B	}�B	�hB	�B	��B	��B
B
+B
8RB
N�B
�B
�jB
�
B
��B-BN�B[#B`BBffBt�Bz�B}�Br�Bu�B� B�+B��BÖB�B�B��B��BB��BB��B��B��B�B�sB�#B��B�qB��B�VB~�Bo�B]/BT�BN�BG�B-B\B
��B
�mB
�B
��B
B
�9B
�7B
+B	��B	�B	ffB	9XB	%�B	uB	+B��B�B�B�mB�;B�
B��B��B��B��B��B��B�
B�TB	JB	
=B	+B	B	+B	bB	�B	2-B	?}B	D�B	D�B	D�B	8RB	&�B	&�B	,B	'�B	�B�B�;B��B��B�9B�!B�3B�?B�^B�}BƨB�
BĜB�jB�jB�qB�wB��BBǮB��B��B�)B�B��B�
B��B�/B�NB�5B�B�B�B�ZB�B��B��B�mB�HB�NB�NB�NB�NB�NB�TB�ZB�ZB�TB�fB�B��B��B��B��B��B��B��B�B�B�B�B��B��B�B�B�B�`B�BB�B��B��B��B�
B�B�B�)B�)B�HB�ZB�B��B��B��B	  B	B	B	B	  B	B	B	%B	VB	uB	 �B	+B	+B	/B	1'B	/B	&�B	$�B	'�B	(�B	(�B	'�B	)�B	.B	0!B	33B	5?B	8RB	8RB	9XB	:^B	<jB	?}B	?}B	A�B	B�B	C�B	D�B	I�B	P�B	Q�B	YB	ZB	[#B	`BB	e`B	k�B	m�B	n�B	o�B	p�B	r�B	t�B	s�B	t�B	x�B	y�B	y�B	y�B	z�B	}�B	~�B	� B	� B	� B	� B	� B	�B	�B	�%B	�=B	�DB	�PB	�PB	�PB	�VB	�oB	�oB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�3B	�LB	�qB	�wB	�wB	�qB	�qB	�wB	�}B	��B	��B	B	ĜB	ŢB	ŢB	ĜB	ŢB	ŢB	ŢB	ŢB	ŢB	ŢB	ŢB	ŢB	ƨB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�)B	�;B	�;B	�;B	�;B	�;B	�;B	�;B	�;B	�BB	�`B	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
B
B
B
%B
1B
	7B
	7B
	7B
	7B

=B

=B

=B
DB
DB
DB
JB
PB
PB
VB
VB
\B
\B
bB
bB
hB
hB
hB
hB
bB
VB
\B
\B
VB
VB
\B
\B
\B
hB
hB
hB
oB
uB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
B
0UB
:�2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 BBBBBBBB  BB#�BD�Bq�Bu�B~�B�7B��B�9B��B��B�5B�`B�B��B��B	B	JB	�B	�B	'�B	=qB	aHB	cTB	hsB	p�B	s�B	}�B	�hB	�B	��B	��B
B
+B
8RB
N�B
�B
�jB
�
B
��B-BN�B[#B`BBffBt�Bz�B}�Br�Bu�B� B�+B��BÖB�B�B��B��BB��BB��B��B��B�B�sB�#B��B�qB��B�VB~�Bo�B]/BT�BN�BG�B-B\B
��B
�mB
�B
��B
B
�9B
�7B
+B	��B	�B	ffB	9XB	%�B	uB	+B��B�B�B�mB�;B�
B��B��B��B��B��B��B�
B�TB	JB	
=B	+B	B	+B	bB	�B	2-B	?}B	D�B	D�B	D�B	8RB	&�B	&�B	,B	'�B	�B�B�;B��B��B�9B�!B�3B�?B�^B�}BƨB�
BĜB�jB�jB�qB�wB��BBǮB��B��B�)B�B��B�
B��B�/B�NB�5B�B�B�B�ZB�B��B��B�mB�HB�NB�NB�NB�NB�NB�TB�ZB�ZB�TB�fB�B��B��B��B��B��B��B��B�B�B�B�B��B��B�B�B�B�`B�BB�B��B��B��B�
B�B�B�)B�)B�HB�ZB�B��B��B��B	  B	B	B	B	  B	B	B	%B	VB	uB	 �B	+B	+B	/B	1'B	/B	&�B	$�B	'�B	(�B	(�B	'�B	)�B	.B	0!B	33B	5?B	8RB	8RB	9XB	:^B	<jB	?}B	?}B	A�B	B�B	C�B	D�B	I�B	P�B	Q�B	YB	ZB	[#B	`BB	e`B	k�B	m�B	n�B	o�B	p�B	r�B	t�B	s�B	t�B	x�B	y�B	y�B	y�B	z�B	}�B	~�B	� B	� B	� B	� B	� B	�B	�B	�%B	�=B	�DB	�PB	�PB	�PB	�VB	�oB	�oB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�3B	�LB	�qB	�wB	�wB	�qB	�qB	�wB	�}B	��B	��B	B	ĜB	ŢB	ŢB	ĜB	ŢB	ŢB	ŢB	ŢB	ŢB	ŢB	ŢB	ŢB	ƨB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�)B	�;B	�;B	�;B	�;B	�;B	�;B	�;B	�;B	�BB	�`B	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
B
B
B
%B
1B
	7B
	7B
	7B
	7B

=B

=B

=B
DB
DB
DB
JB
PB
PB
VB
VB
\B
\B
bB
bB
hB
hB
hB
hB
bB
VB
\B
\B
VB
VB
\B
\B
\B
hB
hB
hB
oB
uB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
B
0UB
:�2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.28 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005190600                              AO  ARCAADJP                                                                    20181005190600    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005190600  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005190600  QCF$                G�O�G�O�G�O�8000            