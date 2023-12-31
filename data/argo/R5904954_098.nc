CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:17:10Z creation      
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
_FillValue                    �0Argo profile    3.1 1.2 19500101000000  20181005191710  20181005191710  5904954 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               bA   AO  6557                            2B  A   APEX                            7468                            062512                          846 @���	J�1   @����QV@4��Q��dB��vȴ1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      bA   A   A   @9��@�  @�  A   A   A@  A`  A�  A�  A�33A�33A�33A�  A�  A���B   BffBffB��B��B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B���B�  B�  B�33B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C�C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C#�fC%�fC(  C*�C,  C.�C0�C2�C4  C6  C8�C:  C<�C>  C@  CB  CD  CF�CH  CJ  CL  CN  CP  CR  CT  CV  CX  CY�fC\  C]�fC`�Cb�Cd  Cf  Ch  Cj�Cl�Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C��3C��C��C��C�  C��3C�  C��3C��C�  C�  C�  C��C�  C�  C�  C��C�  C�  C��3C��3C��C��C��C��C�  C��3C��3C��3C�  C�  C��3C�  C�  C��3C�  C��3C�  C��3C�  C��C��C��C��3C��3C�  C��C�  C�  C��C�  C��3C�  C��C��C�  C��3C�  C��C��C��C�  C��3C��3C�  C�  C�  C��3C��3C�  C�  C��3C��C��C�  C��C�  C�  C��C�  C��3C�  C��C��C��C��3C�  C�  C�  C�  C�  C��C��3C�  C��3C��C��C��C��C��C��C��C��C�  C�  C�  C��3C�  C��C��C��3C�  C��C�  C�  C�  C��C�  C��3C��3C��3C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� DfD�fD  D� D  D� D  D� D��Dy�D  D�fDfD� D  D� D  D� D  D� D  D�fD��D� DfD�fD  D� D  D�fD  D� DfD� D  D� D  Dy�D  D�fD  D� D   D � D ��D!y�D!��D"� D#  D#� D$  D$� D%  D%� D&  D&� D&��D'� D(  D(y�D(��D)� D*  D*� D+  D+� D+��D,� D-fD-� D.  D.� D/fD/�fD0fD0�fD0��D1� D2fD2�fD3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8fD8�fD9fD9�fD:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DAy�DA��DB� DC  DC� DD  DDy�DD��DE�fDFfDF� DG  DGy�DG��DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DMfDM� DN  DN� DOfDO� DP  DP� DQ  DQ� DRfDR� DS  DS� DT  DT� DU  DU� DV  DV� DV��DWy�DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D\��D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db�fDc  Dc� Dd  Dd� De  De� Df  Dfy�Dg  Dg� Dg��Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do�fDpfDp� Dq  Dq� DrfDr�fDs  Ds� Dt  Dt� Du  Du�fDv  Dv� DwfDw� Dw�3Dy��D�?�D��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @K�@���@���Az�A$z�ADz�Adz�A�=qA�=qA�p�A�p�A�p�A�=qA�=qA�
>B�B	�B�B�RB �RB)�B1�B9�BA�BI�BQ�BY�Ba�Bi�Bq�By�B��\B��\B�\)B��\B��\B�B��\B�\)B��\B��\B��\B��\B��\B��\B��\B��\B��\Bď\Bȏ\B̏\BЏ\Bԏ\B؏\B܏\B��\B�\B�\B�\B��\B�\B��\B��\C G�CaHCG�CG�CG�C
G�CG�CG�CG�CG�CG�CG�CG�CG�CG�CG�C G�C"G�C$.C&.C(G�C*aHC,G�C.aHC0aHC2aHC4G�C6G�C8aHC:G�C<aHC>G�C@G�CBG�CDG�CFaHCHG�CJG�CLG�CNG�CPG�CRG�CTG�CVG�CXG�CZ.C\G�C^.C`aHCbaHCdG�CfG�ChG�CjaHClaHCnG�CpG�CrG�CtG�CvG�CxG�CzG�C|G�C~G�C�#�C�#�C�
C�0�C�0�C�0�C�#�C�
C�#�C�
C�0�C�#�C�#�C�#�C�0�C�#�C�#�C�#�C�0�C�#�C�#�C�
C�
C�0�C�0�C�0�C�0�C�#�C�
C�
C�
C�#�C�#�C�
C�#�C�#�C�
C�#�C�
C�#�C�
C�#�C�0�C�=qC�0�C�
C�
C�#�C�0�C�#�C�#�C�0�C�#�C�
C�#�C�0�C�0�C�#�C�
C�#�C�0�C�0�C�0�C�#�C�
C�
C�#�C�#�C�#�C�
C�
C�#�C�#�C�
C�0�C�0�C�#�C�0�C�#�C�#�C�0�C�#�C�
C�#�C�0�C�0�C�0�C�
C�#�C�#�C�#�C�#�C�#�C�0�C�
C�#�C�
C�0�C�=qC�0�C�0�C�=qC�0�C�0�C�0�C�#�C�#�C�#�C�
C�#�C�0�C�0�C�
C�#�C�0�C�#�C�#�C�#�C�0�C�#�C�
C�
C�
C�#�C�#�C�#�C�#�C�#�D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��DRD�RD�D��D�D��D�D��D�D��D�D�RDRD��D�D��D�D��D�D��D�D�RD�D��DRD�RD�D��D�D�RD�D��DRD��D�D��D�D��D�D�RD�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-RD-��D.�D.��D/RD/�RD0RD0�RD1�D1��D2RD2�RD3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8RD8�RD9RD9�RD:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE�RDFRDF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DMRDM��DN�DN��DORDO��DP�DP��DQ�DQ��DRRDR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db�RDc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do�RDpRDp��Dq�Dq��DrRDr�RDs�Ds��Dt�Dt��Du�Du�RDv�Dv��DwRDw��DxDy��D�H�D��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�bNA�ffA�p�A�n�A�l�A�l�A�l�A�jA�hsA�ffA�l�A�l�A�bNA�I�A�dZA�l�A�l�A�ƨA�`BA�Q�A�dZA�~�A�"�A�n�A�A�A�%AîA���A�&�A��-A��A��-A�=qA���A�r�A��/A���A�XA�ZA��^A�5?A��RA���A��hA���A��A��RA�p�A�1'A�C�A���A��A�t�A�A�M�A�1'A�oA��HA�ƨA��\A��wA�n�A�A�A�z�A�l�A�33A��A��yA��A���A���A��jA��A���A�O�A�v�A�"�A�&�A�?}A��-A�hsA�A�=qA���A��/A�A���A�G�A�A�p�A���A���A��A�O�A�A~v�A}��A{XAw�;Ast�Ao�TAl�!Ah�Ag�PAfv�Ad��A_XA\�RAZ�AW�mAT�AS��AP��AO�-AM�mAK%AI��AH��AG��AE�;ACXAB��A@��A?��A>��A;��A:�!A9C�A8bNA6��A5dZA4E�A3�A2�+A2A�A1&�A/A.�DA,-A+;dA*��A)��A'ƨA&Q�A#�PA"A v�A�mAG�A��AZA�hA&�A��An�AE�A(�AbA�A-A;dA�A��AS�A�wAp�AAn�A�mA7LA�AȴA�Ax�AȴA��A�PA
ZA	��A	l�A	
=Av�A  A�^AC�A�uA�A�A/A��AjA�AhsA ȴA ��A ��A M�A �@���@�E�@�@���@��/@��;@�t�@��@��y@��\@���@�C�@�E�@��@�ƨ@���@���@�x�@�X@�%@웦@�I�@�b@땁@�
=@���@�^5@�Ĝ@�  @���@�=q@���@�b@޸R@�5?@ݡ�@�&�@�Z@ڰ!@��@���@��@�z�@�l�@թ�@��@�
=@�@� �@�V@�z�@�r�@ʗ�@��@�J@��@�1'@�C�@�o@§�@�5?@��@��@�o@�Z@��@�$�@�O�@�/@�p�@��#@�+@��H@�b@�v�@��-@� �@�ff@��H@�@�=q@�`B@��-@�K�@��-@���@���@���@���@��-@�-@��@��-@�G�@��9@��D@�z�@�Q�@���@���@�V@�@��@���@�p�@�`B@��@��/@��u@�1'@�"�@�G�@���@�
=@��u@��m@�I�@� �@��w@�
=@�ȴ@��@��y@�ff@��@�@���@��@��\@���@�7L@���@�1@���@��@�=q@��@�Q�@��@��@���@��/@��j@�I�@� �@��@��@�b@�b@�b@��@��m@��@�O�@��u@��@��+@�ff@�n�@��@��H@�V@���@���@�33@�E�@��^@��T@��h@�`B@��-@���@���@��-@��7@��@��@�V@��/@��D@��@���@���@�l�@�;d@�"�@�"�@��@�
=@�@��@�v�@�{@��@�%@�Q�@�(�@� �@��@���@���@��P@�\)@�\)@�C�@�
=@��@���@�ff@�5?@�J@���@�@�x�@���@��u@�  @��@��@�S�@�S�@�33@��@���@��H@���@���@�V@��@�/@��/@���@�r�@�j@�I�@�  @�+@���@�^5@��#@���@�?}@�bN@�(�@��;@��w@���@���@���@���@�t�@�S�@�+@��@��R@�ff@��@���@���@�G�@��@���@���@��/@��@���@���@�%@���@�V@�%@���@��@���@��9@�j@�  @���@�\)@�C�@�33@��@��H@�E�@���@��^@�&�@�V@���@�(�@��
@�l�@�
=@��@�^5@�$�@��-@���@�C@ic@Wƨ1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�bNA�ffA�p�A�n�A�l�A�l�A�l�A�jA�hsA�ffA�l�A�l�A�bNA�I�A�dZA�l�A�l�A�ƨA�`BA�Q�A�dZA�~�A�"�A�n�A�A�A�%AîA���A�&�A��-A��A��-A�=qA���A�r�A��/A���A�XA�ZA��^A�5?A��RA���A��hA���A��A��RA�p�A�1'A�C�A���A��A�t�A�A�M�A�1'A�oA��HA�ƨA��\A��wA�n�A�A�A�z�A�l�A�33A��A��yA��A���A���A��jA��A���A�O�A�v�A�"�A�&�A�?}A��-A�hsA�A�=qA���A��/A�A���A�G�A�A�p�A���A���A��A�O�A�A~v�A}��A{XAw�;Ast�Ao�TAl�!Ah�Ag�PAfv�Ad��A_XA\�RAZ�AW�mAT�AS��AP��AO�-AM�mAK%AI��AH��AG��AE�;ACXAB��A@��A?��A>��A;��A:�!A9C�A8bNA6��A5dZA4E�A3�A2�+A2A�A1&�A/A.�DA,-A+;dA*��A)��A'ƨA&Q�A#�PA"A v�A�mAG�A��AZA�hA&�A��An�AE�A(�AbA�A-A;dA�A��AS�A�wAp�AAn�A�mA7LA�AȴA�Ax�AȴA��A�PA
ZA	��A	l�A	
=Av�A  A�^AC�A�uA�A�A/A��AjA�AhsA ȴA ��A ��A M�A �@���@�E�@�@���@��/@��;@�t�@��@��y@��\@���@�C�@�E�@��@�ƨ@���@���@�x�@�X@�%@웦@�I�@�b@땁@�
=@���@�^5@�Ĝ@�  @���@�=q@���@�b@޸R@�5?@ݡ�@�&�@�Z@ڰ!@��@���@��@�z�@�l�@թ�@��@�
=@�@� �@�V@�z�@�r�@ʗ�@��@�J@��@�1'@�C�@�o@§�@�5?@��@��@�o@�Z@��@�$�@�O�@�/@�p�@��#@�+@��H@�b@�v�@��-@� �@�ff@��H@�@�=q@�`B@��-@�K�@��-@���@���@���@���@��-@�-@��@��-@�G�@��9@��D@�z�@�Q�@���@���@�V@�@��@���@�p�@�`B@��@��/@��u@�1'@�"�@�G�@���@�
=@��u@��m@�I�@� �@��w@�
=@�ȴ@��@��y@�ff@��@�@���@��@��\@���@�7L@���@�1@���@��@�=q@��@�Q�@��@��@���@��/@��j@�I�@� �@��@��@�b@�b@�b@��@��m@��@�O�@��u@��@��+@�ff@�n�@��@��H@�V@���@���@�33@�E�@��^@��T@��h@�`B@��-@���@���@��-@��7@��@��@�V@��/@��D@��@���@���@�l�@�;d@�"�@�"�@��@�
=@�@��@�v�@�{@��@�%@�Q�@�(�@� �@��@���@���@��P@�\)@�\)@�C�@�
=@��@���@�ff@�5?@�J@���@�@�x�@���@��u@�  @��@��@�S�@�S�@�33@��@���@��H@���@���@�V@��@�/@��/@���@�r�@�j@�I�@�  @�+@���@�^5@��#@���@�?}@�bN@�(�@��;@��w@���@���@���@���@�t�@�S�@�+@��@��R@�ff@��@���@���@�G�@��@���@���@��/@��@���@���@�%@���@�V@�%@���@��@���@��9@�j@�  @���@�\)@�C�@�33@��@��H@�E�@���@��^@�&�@�V@���@�(�@��
@�l�@�
=@��@�^5@�$�@��-@���@�C@ic@Wƨ1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B?}B?}B?}B?}B?}B?}B?}B?}B@�B@�B@�B@�B?}B>wB?}B?}B>wB9XB33B �B��B�B$�B>wBH�BP�B]/BdZBhsBl�Bp�Bq�B{�B�PB��B��B��B��B��B��B��B��B��B��B��B��B�\B�JB�7B�By�Bm�BP�B5?B$�B�B  B�B�TB�
B��B�qB�!B��B��B��B�{B�=B�B|�Bv�Bl�Be`BO�B@�B5?B0!B#�B�BJB
��B
�fB
�#B
��B
ŢB
�LB
��B
��B
�PB
u�B
k�B
^5B
ZB
J�B
B�B
49B
-B
�B
B	�fB	ɺB	�?B	��B	�{B	�DB	~�B	dZB	S�B	G�B	:^B	'�B	#�B	�B	oB		7B��B��B�B�B�ZB�#B�B��B��B��BƨBB�}B�dB�FB�9B�'B�B�B�B��B��B��B��B��B��B��B�{B�PB�DB�1B�1B�1B�7B�7B�=B�JB�JB�\B�bB�bB�bB�\B�\B�uB�hB�bB�bB�VB�JB�DB�7B�1B�+B�%B�%B�B�B� B�B�B�B�B�B�B�B�B�B�B�%B�+B�1B�1B�=B�=B�DB�PB�hB�uB�oB�uB�{B�uB�uB�uB��B��B��B��B��B��B��B��B��B��B��B��B��B��B�VB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�'B�'B�'B�'B�B��B��B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�'B�}B�B�B�B�mB�sB�yB�B�B��B��B��B��B	+B	+B	B	%B	1B	\B	�B	�B	%�B	(�B	)�B	-B	.B	.B	/B	1'B	2-B	6FB	6FB	7LB	9XB	<jB	=qB	>wB	B�B	D�B	C�B	?}B	6FB	,B	�B	�B	�B	�B	�B	�B	�B	�B	�B	 �B	#�B	$�B	'�B	+B	33B	7LB	8RB	7LB	?}B	F�B	I�B	I�B	I�B	E�B	F�B	K�B	P�B	R�B	XB	YB	[#B	ZB	\)B	cTB	ffB	ffB	ffB	ffB	hsB	o�B	y�B	y�B	w�B	u�B	w�B	|�B	�B	�B	�1B	�JB	�bB	�uB	�uB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�'B	�3B	�?B	�?B	�?B	�?B	�FB	�FB	�XB	�jB	�qB	�wB	ÖB	ŢB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�#B	�#B	�#B	�#B	�)B	�)B	�/B	�/B	�5B	�5B	�BB	�HB	�HB	�HB	�NB	�NB	�TB	�ZB	�mB	�sB	�yB	�yB	�yB	�yB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
�B
)D2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 B?}B?}B?}B?}B?}B?}B?}B?}B@�B@�B@�B@�B?}B>wB?}B?}B>wB9XB33B �B��B�B$�B>wBH�BP�B]/BdZBhsBl�Bp�Bq�B{�B�PB��B��B��B��B��B��B��B��B��B��B��B��B�\B�JB�7B�By�Bm�BP�B5?B$�B�B  B�B�TB�
B��B�qB�!B��B��B��B�{B�=B�B|�Bv�Bl�Be`BO�B@�B5?B0!B#�B�BJB
��B
�fB
�#B
��B
ŢB
�LB
��B
��B
�PB
u�B
k�B
^5B
ZB
J�B
B�B
49B
-B
�B
B	�fB	ɺB	�?B	��B	�{B	�DB	~�B	dZB	S�B	G�B	:^B	'�B	#�B	�B	oB		7B��B��B�B�B�ZB�#B�B��B��B��BƨBB�}B�dB�FB�9B�'B�B�B�B��B��B��B��B��B��B��B�{B�PB�DB�1B�1B�1B�7B�7B�=B�JB�JB�\B�bB�bB�bB�\B�\B�uB�hB�bB�bB�VB�JB�DB�7B�1B�+B�%B�%B�B�B� B�B�B�B�B�B�B�B�B�B�B�%B�+B�1B�1B�=B�=B�DB�PB�hB�uB�oB�uB�{B�uB�uB�uB��B��B��B��B��B��B��B��B��B��B��B��B��B��B�VB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�'B�'B�'B�'B�B��B��B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�'B�}B�B�B�B�mB�sB�yB�B�B��B��B��B��B	+B	+B	B	%B	1B	\B	�B	�B	%�B	(�B	)�B	-B	.B	.B	/B	1'B	2-B	6FB	6FB	7LB	9XB	<jB	=qB	>wB	B�B	D�B	C�B	?}B	6FB	,B	�B	�B	�B	�B	�B	�B	�B	�B	�B	 �B	#�B	$�B	'�B	+B	33B	7LB	8RB	7LB	?}B	F�B	I�B	I�B	I�B	E�B	F�B	K�B	P�B	R�B	XB	YB	[#B	ZB	\)B	cTB	ffB	ffB	ffB	ffB	hsB	o�B	y�B	y�B	w�B	u�B	w�B	|�B	�B	�B	�1B	�JB	�bB	�uB	�uB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�'B	�3B	�?B	�?B	�?B	�?B	�FB	�FB	�XB	�jB	�qB	�wB	ÖB	ŢB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�#B	�#B	�#B	�#B	�)B	�)B	�/B	�/B	�5B	�5B	�BB	�HB	�HB	�HB	�NB	�NB	�TB	�ZB	�mB	�sB	�yB	�yB	�yB	�yB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
�B
)D2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.28 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005191710                              AO  ARCAADJP                                                                    20181005191710    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005191710  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005191710  QCF$                G�O�G�O�G�O�8000            