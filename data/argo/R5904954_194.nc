CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:17:33Z creation      
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20181005191733  20181005191733  5904954 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6557                            2B  A   APEX                            7468                            062512                          846 @���b1   @���-��@6ffffff�d�n��P1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �A   A   A   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  C   C�fC  C  C  C
  C  C  C  C�C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.�C0  C2  C4�C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cw�fCz  C|  C~  C��C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C��3C�  C�  C��3C�  C��C��C��C��C�  C��3C�  C��3C��3C��3C��C�  C��3C�  C�  C�  C�  C�  C�  C��3C��3C��C��C�  C��C��C�  C��C�  C��3C�  C��3C�  C�  C��C�  C��C��C��C�  C�  C�  C��C��3C�  C�  C�  C�  C�  C�  C�  C��C��C�  C��3C�  C��3C��3C��C��fC��3C�  C�  C�  C��C��C��C�  C��C�  C�  C��3C�  C�  C��3C�  C��C��C�  C�  C�  C��C�  C��3C�  C��3C�  C�  C��C��C��C��C��3C�  C��3C�  C�  C�  D fD � D  D� DfD�fD��D� DfD�fDfD�fDfD� D��D� DfD� D��D	y�D	��D
� DfD� D  D� D�3Dy�D��D� DfD� D  D�fD  D� DfD��D�D�fDfD� D��Dy�D  Dy�D  D� DfD�fD  D�fDfD�fD  Dy�D��D� DfDy�D��Dy�DfD�fD fD �fD!  D!� D"  D"� D#  D#� D$  D$� D%fD%� D&  D&� D'fD'� D(  D(y�D)  D)� D)��D*� D+  D+y�D+��D,� D-  D-� D.  D.y�D.��D/� D0fD0�fD1  D1� D2  D2� D3  D3y�D4  D4�fD5  D5� D6fD6� D6��D7� D7��D8y�D9  D9� D9��D:� D;  D;� D<fD<� D=  D=� D>  D>� D?fD?�fD@fD@� DA  DA� DB  DB�fDC  DCy�DD  DD� DE  DE� DF  DF� DG  DG�fDH  DHy�DI  DI�fDJ  DJ� DK  DK�fDLfDL�fDM  DM� DM��DNy�DO  DO�fDPfDP�fDQ  DQ�fDR  DR� DR��DSy�DS��DTy�DT��DU� DU��DV� DW  DW� DX  DX� DYfDY� DY��DZy�D[  D[�fD\  D\�fD]fD]�fD^  D^�fD_fD_�fD`  D`y�Da  Da�fDb  Db� Dc  Dc�fDdfDd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj�fDkfDk�fDl  Dly�Dl��Dmy�Dn  Dn� Do  Doy�Do��Dpy�Dp��Dqy�Dq��Dr�fDsfDsy�Dt  Dty�Du  Du� Dv  Dv� Dw  Dw� Dw� Dy��D�C3D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�\)@���Az�A$z�ADz�Adz�A�=qA�=qA�=qA�=qA�=qA�=qA�=qA�=qB�B	�B�B�B!�B)�B1�B9�BA�BI�BQ�BY�Ba�Bi�Bq�By�B��\B��\B��\B��\B��\B��\B��\B��\B��\B��\B��\B��\B��\B��\B��\B��\B��\Bď\Bȏ\B�\)BЏ\Bԏ\B؏\B܏\B��\B�\B�B�\B��\B�\B��\B��\C G�C.CG�CG�CG�C
G�CG�CG�CG�CaHCG�CG�CG�CG�CG�CG�C G�C"G�C$G�C&G�C(G�C*G�C,G�C.aHC0G�C2G�C4aHC6G�C8G�C:G�C<G�C>G�C@G�CBG�CDG�CFG�CHG�CJG�CLG�CNG�CPG�CRG�CTG�CVG�CXG�CZG�C\G�C^G�C`G�CbG�CdG�CfG�ChG�CjG�ClG�CnG�CpG�CrG�CtG�CvG�Cx.CzG�C|G�C~G�C�0�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�0�C�0�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�0�C�0�C�#�C�#�C�#�C�#�C�#�C�
C�#�C�#�C�
C�#�C�0�C�0�C�0�C�0�C�#�C�
C�#�C�
C�
C�
C�0�C�#�C�
C�#�C�#�C�#�C�#�C�#�C�#�C�
C�
C�0�C�0�C�#�C�0�C�0�C�#�C�0�C�#�C�
C�#�C�
C�#�C�#�C�=qC�#�C�0�C�0�C�0�C�#�C�#�C�#�C�0�C�
C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�0�C�0�C�#�C�
C�#�C�
C�
C�0�C�
=C�
C�#�C�#�C�#�C�0�C�0�C�0�C�#�C�0�C�#�C�#�C�
C�#�C�#�C�
C�#�C�0�C�=qC�#�C�#�C�#�C�0�C�#�C�
C�#�C�
C�#�C�#�C�0�C�0�C�0�C�0�C�
C�#�C�
C�#�C�#�C�#�D RD ��D�D��DRD�RD�D��DRD�RDRD�RDRD��D�D��DRD��D	�D	��D
�D
��DRD��D�D��DD��D�D��DRD��D�D�RD�D��DRD��D�D�RDRD��D�D��D�D��D�D��DRD�RD�D�RDRD�RD�D��D�D��DRD��D�D��DRD�RD RD �RD!�D!��D"�D"��D#�D#��D$�D$��D%RD%��D&�D&��D'RD'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0RD0�RD1�D1��D2�D2��D3�D3��D4�D4�RD5�D5��D6RD6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<RD<��D=�D=��D>�D>��D?RD?�RD@RD@��DA�DA��DB�DB�RDC�DC��DD�DD��DE�DE��DF�DF��DG�DG�RDH�DH��DI�DI�RDJ�DJ��DK�DK�RDLRDL�RDM�DM��DN�DN��DO�DO�RDPRDP�RDQ�DQ�RDR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DYRDY��DZ�DZ��D[�D[�RD\�D\�RD]RD]�RD^�D^�RD_RD_�RD`�D`��Da�Da�RDb�Db��Dc�Dc�RDdRDd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj�RDkRDk�RDl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr�RDsRDs��Dt�Dt��Du�Du��Dv�Dv��Dw�Dw��Dw��Dy��D�L)D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�n�A�n�A�n�A�n�A�r�A�l�A�n�A�r�A�p�A�hsA�K�A�5?A�
=A��mA��#A���A���A���A͝�A�bNA�G�A�;dA�5?A�33A�1'A�+A��A��A�bA���A�A�%A���A��A��HA��
A���A�ƨA���A̺^A̴9A̧�Ȁ\ÁA�v�A�l�A�ffA�bNA�bNA�bNA�\)A�XA�K�A�-AˮA��#Aʙ�A�"�A�7LAǋDA��A��jA�n�A�A���A���A��A�"�A�A���A���A��mA��yA��/A��
A��A��^A�ĜA��\A�t�A�S�A�oA�XA�G�A�ĜA�oA��mA�ĜA�K�A�A�A�ƨA�E�A��hA�A��A��FA��A�&�A���A���A���A�M�A��
A�
=A�z�A���A�JA���A�(�A�XA�A���A�ȴA���A��A�n�A��A��A���A�hsA��A�K�A���A�1A�^5A��RA��A�5?A�A~��A~��A}��A}%A|$�A{
=Azv�AyS�Aw�Au�#At��AtM�Ar�AnE�AkO�Ah1'Af{Ad�Aa�
A^�HA[�TAYƨAW�-AV��AVM�AT��ATVAT$�AS�^ASS�ARȴARM�AQdZAP��AO�#AN��ANVAMS�AL��AJ5?AHM�AG�AG��AF�`AD��AC��AB�/AB(�AAl�A?�A=��A:�`A9\)A81'A6��A4�\A2�A2~�A2M�A21'A0��A/p�A.�HA.(�A-�TA,��A+��A*��A)�A(�!A(9XA'�^A'oA&{A$��A#�wA"Q�A!|�A!�A �A ��A $�A��A�AJA�AĜA�#AZA-A�A�mAO�A
=A �A�A��A�A�DA��A
�yA
ZA	�hAt�A��A�A�;AK�A�/AbA
=A�A��A�A ��A {@���@��@�dZ@��R@�&�@���@��@��+@���@���@���@��@��^@�x�@���@�9X@���@��`@���@��H@柾@噚@�(�@���@◍@�&�@��@�A�@��@�v�@ٙ�@���@؋D@�j@�A�@��@�ƨ@�E�@�?}@���@��/@��/@���@ԓu@�Z@ӕ�@�{@�I�@��@�{@�?}@̋D@˾w@ʗ�@�=q@���@ɑh@�X@ȴ9@��;@�K�@Ƨ�@�O�@ċD@�A�@�S�@��@��@��9@��@���@��H@��T@��7@�&�@�z�@��P@�{@��@��@���@���@��!@�n�@�@�%@�Q�@�\)@��@�{@���@���@�O�@��`@�9X@���@���@�\)@�K�@�33@���@��h@��D@�\)@�M�@��@��T@�@�V@� �@��
@���@��
@��@��m@�l�@��@�v�@���@�`B@��7@���@���@��#@��#@���@��@���@���@�9X@���@���@���@��P@��@���@�-@��T@���@���@�p�@�O�@��@��/@��D@��w@�S�@���@�ff@�$�@���@���@��7@�x�@�&�@�Ĝ@���@��u@�z�@�Z@�9X@���@��;@��F@���@�K�@�
=@���@�E�@�J@��#@���@�@��-@���@�x�@��`@� �@��w@��@�l�@�
=@�ȴ@���@���@�~�@�V@�-@�$�@�J@��-@���@�bN@�b@���@�"�@�@��H@���@��\@�ff@��@���@��h@�`B@�?}@��@�z�@��m@�ƨ@�ƨ@���@�ƨ@��w@��@�l�@�C�@�
=@��y@��!@�~�@�E�@���@�p�@��/@��u@�A�@��
@�t�@�K�@�33@�o@��H@���@�ff@�=q@�J@��@��@��7@�V@��@�z�@�r�@�j@�bN@�I�@�1@���@�l�@�B�@z�s@j�b111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�n�A�n�A�n�A�n�A�r�A�l�A�n�A�r�A�p�A�hsA�K�A�5?A�
=A��mA��#A���A���A���A͝�A�bNA�G�A�;dA�5?A�33A�1'A�+A��A��A�bA���A�A�%A���A��A��HA��
A���A�ƨA���A̺^A̴9A̧�Ȁ\ÁA�v�A�l�A�ffA�bNA�bNA�bNA�\)A�XA�K�A�-AˮA��#Aʙ�A�"�A�7LAǋDA��A��jA�n�A�A���A���A��A�"�A�A���A���A��mA��yA��/A��
A��A��^A�ĜA��\A�t�A�S�A�oA�XA�G�A�ĜA�oA��mA�ĜA�K�A�A�A�ƨA�E�A��hA�A��A��FA��A�&�A���A���A���A�M�A��
A�
=A�z�A���A�JA���A�(�A�XA�A���A�ȴA���A��A�n�A��A��A���A�hsA��A�K�A���A�1A�^5A��RA��A�5?A�A~��A~��A}��A}%A|$�A{
=Azv�AyS�Aw�Au�#At��AtM�Ar�AnE�AkO�Ah1'Af{Ad�Aa�
A^�HA[�TAYƨAW�-AV��AVM�AT��ATVAT$�AS�^ASS�ARȴARM�AQdZAP��AO�#AN��ANVAMS�AL��AJ5?AHM�AG�AG��AF�`AD��AC��AB�/AB(�AAl�A?�A=��A:�`A9\)A81'A6��A4�\A2�A2~�A2M�A21'A0��A/p�A.�HA.(�A-�TA,��A+��A*��A)�A(�!A(9XA'�^A'oA&{A$��A#�wA"Q�A!|�A!�A �A ��A $�A��A�AJA�AĜA�#AZA-A�A�mAO�A
=A �A�A��A�A�DA��A
�yA
ZA	�hAt�A��A�A�;AK�A�/AbA
=A�A��A�A ��A {@���@��@�dZ@��R@�&�@���@��@��+@���@���@���@��@��^@�x�@���@�9X@���@��`@���@��H@柾@噚@�(�@���@◍@�&�@��@�A�@��@�v�@ٙ�@���@؋D@�j@�A�@��@�ƨ@�E�@�?}@���@��/@��/@���@ԓu@�Z@ӕ�@�{@�I�@��@�{@�?}@̋D@˾w@ʗ�@�=q@���@ɑh@�X@ȴ9@��;@�K�@Ƨ�@�O�@ċD@�A�@�S�@��@��@��9@��@���@��H@��T@��7@�&�@�z�@��P@�{@��@��@���@���@��!@�n�@�@�%@�Q�@�\)@��@�{@���@���@�O�@��`@�9X@���@���@�\)@�K�@�33@���@��h@��D@�\)@�M�@��@��T@�@�V@� �@��
@���@��
@��@��m@�l�@��@�v�@���@�`B@��7@���@���@��#@��#@���@��@���@���@�9X@���@���@���@��P@��@���@�-@��T@���@���@�p�@�O�@��@��/@��D@��w@�S�@���@�ff@�$�@���@���@��7@�x�@�&�@�Ĝ@���@��u@�z�@�Z@�9X@���@��;@��F@���@�K�@�
=@���@�E�@�J@��#@���@�@��-@���@�x�@��`@� �@��w@��@�l�@�
=@�ȴ@���@���@�~�@�V@�-@�$�@�J@��-@���@�bN@�b@���@�"�@�@��H@���@��\@�ff@��@���@��h@�`B@�?}@��@�z�@��m@�ƨ@�ƨ@���@�ƨ@��w@��@�l�@�C�@�
=@��y@��!@�~�@�E�@���@�p�@��/@��u@�A�@��
@�t�@�K�@�33@�o@��H@���@�ff@�=q@�J@��@��@��7@�V@��@�z�@�r�@�j@�bN@�I�@�1@���@�l�@�B�@z�s@j�b111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�)B�)B�)B�)B�)B�/B�/B�)B�/B�;B�ZB�sB�B�B�B��BBB	7B\BoBuB{B{B�B�B�B �B"�B�B$�B(�B,B/B2-B5?B8RB:^B;dB<jB=qB?}BA�BG�BO�BO�BP�BP�BQ�BR�BR�BR�BQ�BP�BS�B\)B^5BjB�%B��B��B�%B~�Bx�B� B�=B�PB�bB�uB��B��B��B��B��B�B�FBÖBB�wB�wB�9B�B��B��B��B��B��B��B��B��B�uB�DB{�BiyB[#BO�B?}B5?B$�BuB1B��B�B�`B�;B�
B��B�}B�FB�B��B�=Bs�BZBH�B-BuB	7BB
��B
�fB
��B
��B
�FB
�B
��B
��B
�bB
�+B
�B
~�B
y�B
r�B
l�B
e`B
`BB
W
B
J�B
?}B
9XB
49B
(�B
1B	�B	�5B	��B	��B	�'B	��B	�VB	�B	t�B	l�B	iyB	aHB	^5B	]/B	ZB	XB	T�B	R�B	N�B	J�B	G�B	D�B	A�B	=qB	8RB	1'B	(�B	&�B	$�B	!�B	�B	hB	JB	+B	B��B�B�sB�NB�5B�B��B��B��B��BȴBŢB��B�wB�dB�^B�RB�FB�9B�-B�B�B�B��B��B��B��B��B��B��B��B�{B�oB�VB�7B�B}�By�Bw�Bs�Bo�Bm�BjBiyBgmBffBdZBdZBbNBaHB]/BXBVBVBYBXBW
BXBYBYB\)B\)BZBXBVBVBVBW
BW
BVBT�BVBVBT�BS�BR�BP�BO�BQ�BR�BR�BR�BR�BQ�BR�BR�BR�BQ�BQ�BS�BR�BR�BQ�BS�BR�BS�BS�BVBXBYBZBZB[#B[#BbNBe`BgmBgmBgmBhsBhsBgmBhsBjBn�Br�Bs�Bs�Bt�Bx�B~�B�B�B�B�B�1B�VB�oB��B��B��B��B��B��B�B�XB�qB�}BÖB��B��B��B��B�
B�;B�NB�mB�B�B�B�B��B��B��B��B	B	1B	
=B	JB	VB	hB	�B	�B	"�B	#�B	%�B	(�B	0!B	5?B	49B	1'B	5?B	6FB	7LB	8RB	;dB	A�B	F�B	G�B	G�B	I�B	M�B	T�B	VB	T�B	VB	ZB	]/B	_;B	aHB	aHB	dZB	e`B	l�B	m�B	m�B	o�B	p�B	r�B	r�B	s�B	v�B	w�B	|�B	~�B	� B	�B	�B	�B	�B	�+B	�1B	�1B	�1B	�+B	�+B	�1B	�1B	�7B	�7B	�=B	�JB	�\B	�bB	�hB	�hB	�oB	�uB	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�-B	�3B	�9B	�9B	�?B	�FB	�RB	�XB	�^B	�^B	�jB	�jB	�wB	�}B	ĜB	ǮB	ȴB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�B	�/B	�/B	�;B	�BB	�BB	�NB	�TB	�TB	�ZB	�ZB	�`B	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B
	lB
�222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  B�)B�)B�)B�)B�)B�/B�/B�)B�/B�;B�ZB�sB�B�B�B��BBB	7B\BoBuB{B{B�B�B�B �B"�B�B$�B(�B,B/B2-B5?B8RB:^B;dB<jB=qB?}BA�BG�BO�BO�BP�BP�BQ�BR�BR�BR�BQ�BP�BS�B\)B^5BjB�%B��B��B�%B~�Bx�B� B�=B�PB�bB�uB��B��B��B��B��B�B�FBÖBB�wB�wB�9B�B��B��B��B��B��B��B��B��B�uB�DB{�BiyB[#BO�B?}B5?B$�BuB1B��B�B�`B�;B�
B��B�}B�FB�B��B�=Bs�BZBH�B-BuB	7BB
��B
�fB
��B
��B
�FB
�B
��B
��B
�bB
�+B
�B
~�B
y�B
r�B
l�B
e`B
`BB
W
B
J�B
?}B
9XB
49B
(�B
1B	�B	�5B	��B	��B	�'B	��B	�VB	�B	t�B	l�B	iyB	aHB	^5B	]/B	ZB	XB	T�B	R�B	N�B	J�B	G�B	D�B	A�B	=qB	8RB	1'B	(�B	&�B	$�B	!�B	�B	hB	JB	+B	B��B�B�sB�NB�5B�B��B��B��B��BȴBŢB��B�wB�dB�^B�RB�FB�9B�-B�B�B�B��B��B��B��B��B��B��B��B�{B�oB�VB�7B�B}�By�Bw�Bs�Bo�Bm�BjBiyBgmBffBdZBdZBbNBaHB]/BXBVBVBYBXBW
BXBYBYB\)B\)BZBXBVBVBVBW
BW
BVBT�BVBVBT�BS�BR�BP�BO�BQ�BR�BR�BR�BR�BQ�BR�BR�BR�BQ�BQ�BS�BR�BR�BQ�BS�BR�BS�BS�BVBXBYBZBZB[#B[#BbNBe`BgmBgmBgmBhsBhsBgmBhsBjBn�Br�Bs�Bs�Bt�Bx�B~�B�B�B�B�B�1B�VB�oB��B��B��B��B��B��B�B�XB�qB�}BÖB��B��B��B��B�
B�;B�NB�mB�B�B�B�B��B��B��B��B	B	1B	
=B	JB	VB	hB	�B	�B	"�B	#�B	%�B	(�B	0!B	5?B	49B	1'B	5?B	6FB	7LB	8RB	;dB	A�B	F�B	G�B	G�B	I�B	M�B	T�B	VB	T�B	VB	ZB	]/B	_;B	aHB	aHB	dZB	e`B	l�B	m�B	m�B	o�B	p�B	r�B	r�B	s�B	v�B	w�B	|�B	~�B	� B	�B	�B	�B	�B	�+B	�1B	�1B	�1B	�+B	�+B	�1B	�1B	�7B	�7B	�=B	�JB	�\B	�bB	�hB	�hB	�oB	�uB	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�-B	�3B	�9B	�9B	�?B	�FB	�RB	�XB	�^B	�^B	�jB	�jB	�wB	�}B	ĜB	ǮB	ȴB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�B	�/B	�/B	�;B	�BB	�BB	�NB	�TB	�TB	�ZB	�ZB	�`B	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B
	lB
�222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.28 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005191733                              AO  ARCAADJP                                                                    20181005191733    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005191733  QCP$                G�O�G�O�G�O�F03E            AO  ARGQQCPL                                                                    20181005191733  QCF$                G�O�G�O�G�O�8000            