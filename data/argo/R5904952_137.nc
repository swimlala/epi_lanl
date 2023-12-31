CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:05:35Z creation      
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
resolution        =���   axis      Z        l  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  @�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     l  B�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J$   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     l  L    TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  Sl   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  Z�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  \�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  d    TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  e�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  mh   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  t�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  v�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ~   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �d   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �    HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �$   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �4   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �8   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �<   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �@Argo profile    3.1 1.2 19500101000000  20181005190535  20181005190535  5904952 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6431                            2B  A   APEX                            7466                            062512                          846 @�۩v��W1   @�۪�|@0�$�/�c�(�\1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �A   A   A   @�33@�  A��A   A@  A`  A�  A���A�  A�33A�  A�  A�  A�  B   B  B��B��B ffB(ffB0ffB7��B@  BH  BP  BW��B`  Bh  BpffBxffB�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B���B���B�  B�33B�33B�  B���B�ffB���B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C�fC  C
  C  C  C  C  C�C  C  C  C  C  C   C"  C$  C&  C'�fC*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|�C~  C��C�  C�  C��C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C��3C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C��C��C�  C�  C��C�  C�  C�  C�  C��3C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C��C��C�  C�  C�  D   D � D  D� D  D� D��D� D  D� D  D� DfD� D��D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  Dy�D  D� D��D� D  D� D��Dy�D  D� D��Dy�D  D� DfD�fDfD� D��Dy�D  D� D  D� D  Dy�D��Dy�D  D�fDfD� D��Dy�D   D � D!  D!� D"  D"� D#  D#�fD$  D$� D%  D%� D&  D&� D'  D'� D(  D(y�D(��D)� D*fD*� D+  D+� D,  D,� D,��D-� D.fD.�fD/  D/� D0  D0y�D1  D1� D2  D2� D3fD3� D4  D4�fD5fD5� D6  D6� D6��D7y�D7��D8� D9  D9�fD:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA�fDBfDB� DC  DC�fDDfDD�fDEfDE�fDF  DFy�DF��DGy�DG��DHy�DI  DI� DJ  DJ� DK  DK�fDL  DL� DM  DM� DN  DN� DO  DO� DP  DP�fDQ  DQy�DQ��DR� DR��DS� DTfDT�fDU  DUy�DV  DV� DW  DW� DX  DX� DY  DY� DY��DZ� D[  D[� D[��D\� D]  D]y�D^  D^� D_  D_� D_��D`y�Da  Da�fDb  Db� DcfDc� Dd  Dd�fDe  De� De��Df� Dg  Dg� DhfDh� Di  Di� Dj  Dj� Dj��Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Dsy�Dt  Dt� Du  Du� DvfDv�fDwfDw�fDw��Dy��D�*=D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�(�@���A{A$z�ADz�Adz�A�=qA�
>A�=qA�p�A�=qA�=qA�=qA�=qB�B	�B�RB�RB!�B)�B1�B8�RBA�BI�BQ�BX�RBa�Bi�Bq�By�B��\B��\B��\B��\B��\B��\B��\B��\B��\B��\B��\B��\B��\B�B�\)B�\)B��\B�B�B̏\B�\)B���B�\)B܏\B��\B�\B�\B�\B��\B�\B��\B��\C G�CG�CG�C.CG�C
G�CG�CG�CG�CG�CaHCG�CG�CG�CG�CG�C G�C"G�C$G�C&G�C(.C*G�C,G�C.G�C0G�C2G�C4G�C6G�C8G�C:G�C<G�C>G�C@G�CBG�CDG�CFG�CHG�CJG�CLG�CNG�CPG�CRG�CTG�CVG�CXG�CZG�C\G�C^G�C`G�CbG�CdG�CfG�ChG�CjG�ClG�CnG�CpG�CrG�CtG�CvG�CxG�CzG�C|aHC~G�C�0�C�#�C�#�C�0�C�#�C�
C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�
C�#�C�#�C�
C�0�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�0�C�#�C�#�C�#�C�#�C�0�C�#�C�#�C�#�C�#�C�#�C�#�C�
C�#�C�#�C�#�C�0�C�0�C�#�C�#�C�0�C�#�C�#�C�#�C�#�C�
C�#�C�#�C�#�C�#�C�
C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�0�C�#�C�#�C�0�C�0�C�#�C�#�C�#�D �D ��D�D��D�D��D�D��D�D��D�D��DRD��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��DRD�RDRD��D�D��D�D��D�D��D�D��D�D��D�D�RDRD��D�D��D �D ��D!�D!��D"�D"��D#�D#�RD$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*RD*��D+�D+��D,�D,��D-�D-��D.RD.�RD/�D/��D0�D0��D1�D1��D2�D2��D3RD3��D4�D4�RD5RD5��D6�D6��D7�D7��D8�D8��D9�D9�RD:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA�RDBRDB��DC�DC�RDDRDD�RDERDE�RDF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK�RDL�DL��DM�DM��DN�DN��DO�DO��DP�DP�RDQ�DQ��DR�DR��DS�DS��DTRDT�RDU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da�RDb�Db��DcRDc��Dd�Dd�RDe�De��Df�Df��Dg�Dg��DhRDh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Du�Du��DvRDv�RDwRDw�RDx�Dy�qD�33D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�XA�ZA�XA�ZA�ZA�\)A�^5A�^5A�`BA�`BA�bNA�bNA�dZA�ffA�ffA�hsA�hsA�l�A�l�A�n�A�n�A�n�A�n�A�p�A�r�A�r�A�t�A�x�A�x�A�z�A�z�A�x�A�|�A�~�AҁA҅A҃A҅A҃A҅A҃A҃A�x�A�v�A�z�A�A�9XAϙ�A�9XA�=qA�$�A�p�A�K�A��yA�Q�A�VA�G�A���A���A��`A���A��A��/A��PA���A��A�bNA��uA�\)A�A���A��uA��A��jA���A�I�A�l�A��HA�ƨA�M�A�|�A���A��-A� �A���A��A��!A�~�A�M�A� �A���A�33A�l�A��A�A��#A���A�/A��yA�ffA���A�C�A��A��jA���A�ƨA�l�A�r�A�A}��A{�Ay�wAv��Ar5?An��Af��Ac�wA]�FA[�TAYO�AT �ASVAQ��APA�ANffALjAI+AG�mAGoAE�AD�\AB��AA�TA@��A?O�A>��A=\)A<n�A:��A8�9A7�#A6�A5��A5%A3oA0z�A.=qA-�^A-&�A,�9AA�A��A^5A$�A�!A�A9XAO�At�AM�A��A�-A?}A
�uA
1A	"�A�RA$�A\)A��Ax�A�/A��A|�A33A��A�AS�AA=qA��@��@��@��-@��^@���@�1'@��@��y@���@��\@�^5@�ff@�E�@��T@���@��@�M�@�Ĝ@���@�9@�{@�p�@�I�@�P@���@�=q@�&�@�@��u@���@ܛ�@�v�@ّh@�r�@�l�@�K�@�+@ְ!@��@�Z@�n�@��@���@ӍP@���@�r�@�I�@�A�@�b@���@�
=@̋D@�o@�dZ@�z�@�b@�\)@�5?@Ǯ@��y@�r�@�X@�Z@���@ēu@���@�b@öF@��m@���@��#@��7@�hs@�p�@�x�@��@�9X@��;@�|�@�ƨ@�l�@�o@���@�/@�j@�Q�@�Z@�I�@� �@�dZ@�\)@�l�@�
=@���@���@���@�(�@�Z@��;@��;@��9@��j@��u@�Z@�(�@���@�dZ@��\@��T@��h@��@���@���@���@���@�z�@�ƨ@�C�@��!@�=q@��#@���@�p�@�&�@�Z@��m@��@��@���@���@��h@���@�9X@�t�@�
=@��@�~�@�-@�/@��@�1'@�1@���@�  @�ƨ@���@�dZ@�o@�ȴ@�n�@�$�@��h@���@��9@�I�@�b@��
@��@�dZ@�\)@�+@���@���@��\@�E�@��@���@��@��T@���@�%@�1'@�ƨ@�+@��R@�ff@�E�@��@�@�G�@�%@���@�r�@�1@��m@�b@��@�1'@���@���@��u@���@��@�Ĝ@�j@��@���@��D@�Z@��@��F@��@�ff@�-@�@��@���@���@�O�@�/@��/@�z�@���@�dZ@�+@��y@�ȴ@���@��!@���@��!@��\@�n�@�-@�/@���@��/@��@�/@�G�@�?}@��@���@�1@�1'@�j@�Q�@� �@��m@��@�\)@�;d@�+@��y@���@��+@�=q@���@�p�@��7@��7@��@���@��@�(�@��m@���@��w@���@��@�\)@�+@��R@��+@�^5@�=q@�J@���@�7L@��/@���@�Z@�A�@��@��m@��w@���@�|�@�;d@���@��R@��@��-@��7@�p�@�7L@��@��@��9@���@�j@���@y}�@i��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�XA�ZA�XA�ZA�ZA�\)A�^5A�^5A�`BA�`BA�bNA�bNA�dZA�ffA�ffA�hsA�hsA�l�A�l�A�n�A�n�A�n�A�n�A�p�A�r�A�r�A�t�A�x�A�x�A�z�A�z�A�x�A�|�A�~�AҁA҅A҃A҅A҃A҅A҃A҃A�x�A�v�A�z�A�A�9XAϙ�A�9XA�=qA�$�A�p�A�K�A��yA�Q�A�VA�G�A���A���A��`A���A��A��/A��PA���A��A�bNA��uA�\)A�A���A��uA��A��jA���A�I�A�l�A��HA�ƨA�M�A�|�A���A��-A� �A���A��A��!A�~�A�M�A� �A���A�33A�l�A��A�A��#A���A�/A��yA�ffA���A�C�A��A��jA���A�ƨA�l�A�r�A�A}��A{�Ay�wAv��Ar5?An��Af��Ac�wA]�FA[�TAYO�AT �ASVAQ��APA�ANffALjAI+AG�mAGoAE�AD�\AB��AA�TA@��A?O�A>��A=\)A<n�A:��A8�9A7�#A6�A5��A5%A3oA0z�A.=qA-�^A-&�A,�9AA�A��A^5A$�A�!A�A9XAO�At�AM�A��A�-A?}A
�uA
1A	"�A�RA$�A\)A��Ax�A�/A��A|�A33A��A�AS�AA=qA��@��@��@��-@��^@���@�1'@��@��y@���@��\@�^5@�ff@�E�@��T@���@��@�M�@�Ĝ@���@�9@�{@�p�@�I�@�P@���@�=q@�&�@�@��u@���@ܛ�@�v�@ّh@�r�@�l�@�K�@�+@ְ!@��@�Z@�n�@��@���@ӍP@���@�r�@�I�@�A�@�b@���@�
=@̋D@�o@�dZ@�z�@�b@�\)@�5?@Ǯ@��y@�r�@�X@�Z@���@ēu@���@�b@öF@��m@���@��#@��7@�hs@�p�@�x�@��@�9X@��;@�|�@�ƨ@�l�@�o@���@�/@�j@�Q�@�Z@�I�@� �@�dZ@�\)@�l�@�
=@���@���@���@�(�@�Z@��;@��;@��9@��j@��u@�Z@�(�@���@�dZ@��\@��T@��h@��@���@���@���@���@�z�@�ƨ@�C�@��!@�=q@��#@���@�p�@�&�@�Z@��m@��@��@���@���@��h@���@�9X@�t�@�
=@��@�~�@�-@�/@��@�1'@�1@���@�  @�ƨ@���@�dZ@�o@�ȴ@�n�@�$�@��h@���@��9@�I�@�b@��
@��@�dZ@�\)@�+@���@���@��\@�E�@��@���@��@��T@���@�%@�1'@�ƨ@�+@��R@�ff@�E�@��@�@�G�@�%@���@�r�@�1@��m@�b@��@�1'@���@���@��u@���@��@�Ĝ@�j@��@���@��D@�Z@��@��F@��@�ff@�-@�@��@���@���@�O�@�/@��/@�z�@���@�dZ@�+@��y@�ȴ@���@��!@���@��!@��\@�n�@�-@�/@���@��/@��@�/@�G�@�?}@��@���@�1@�1'@�j@�Q�@� �@��m@��@�\)@�;d@�+@��y@���@��+@�=q@���@�p�@��7@��7@��@���@��@�(�@��m@���@��w@���@��@�\)@�+@��R@��+@�^5@�=q@�J@���@�7L@��/@���@�Z@�A�@��@��m@��w@���@�|�@�;d@���@��R@��@��-@��7@�p�@�7L@��@��@��9@���@�j@���@y}�@i��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
�#B
�B
�#B
�B
�B
�B
�B
�B
�B
�#B
�#B
�#B
�#B
�#B
�#B
�#B
�#B
�#B
�)B
�#B
�#B
�)B
�)B
�)B
�/B
�/B
�)B
�/B
�)B
�/B
�/B
�)B
�/B
�/B
�5B
�5B
�5B
�5B
�/B
�5B
�/B
�)B
�#B
�)B
�TB
��B
��B
��BoB�B
ŢB
��B
�dB
�Bm�B��B��B��BPB{B"�B.B6FB:^B?}BC�BB�B,B#�B!�B49BdZBjBhsBZBG�B;dBB�B@�B-B�B%BB��B�
BB�B��Bt�BW
BI�B@�B2-BhB  B
�mB
��B
�}B
�B
�{B
p�B
dZB
M�B
)�B
bB
B	��B	�;B	��B	�-B	��B	�oB	y�B	W
B	?}B	uB��B�B��B�qB��B��B��B��B�uB�oB�7B�%B�B�B�B�B�B�B�B�B�%B�%B�1B�PB�VB�bB�uB�uB��B��B�B�'B�9B�FB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�LB�jB�wBÖB��B�B�fB�B�B�B�ZB�
B��B��B��B�
B�
B�
B�
B�B�
B�BB�B�B�B�B�B�B�B�B�B�B�B�B�yB�mB�mB�`B�HB�#B�B�
B�B�B�/B�5B�BB�HB�fB�fB�sB��B	1B	JB	JB	PB	PB	hB	�B	�B	�B	�B	�B	�B	�B	(�B	+B	)�B	(�B	"�B	#�B	2-B	(�B	(�B	0!B	5?B	8RB	>wB	A�B	D�B	B�B	@�B	A�B	B�B	F�B	J�B	K�B	N�B	O�B	R�B	[#B	^5B	_;B	_;B	^5B	_;B	`BB	`BB	`BB	`BB	bNB	ffB	iyB	m�B	n�B	q�B	o�B	n�B	r�B	t�B	w�B	� B	�B	�B	�B	�B	�B	�%B	�B	�B	�%B	�1B	�1B	�=B	�PB	�bB	�hB	�oB	�oB	�oB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�3B	�?B	�LB	�LB	�LB	�LB	�RB	�RB	�XB	�dB	�jB	�qB	�wB	�wB	�}B	��B	��B	��B	��B	B	B	ÖB	ÖB	ĜB	ŢB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�/B	�5B	�BB	�HB	�NB	�fB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B	��B
  B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
+B
1B
DB
DB
DB

=B
DB
DB
DB
DB
DB
DB
DB
DB
JB
JB
PB
PB
PB
PB
PB
PB
VB
\B
\B
bB
bB
bB
bB
hB
hB
hB
hB
hB
hB
oB
oB
oB
oB
oB
uB
uB
uB
{B
2B
%,B
-�2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 B
�#B
�B
�#B
�B
�B
�B
�B
�B
�B
�#B
�#B
�#B
�#B
�#B
�#B
�#B
�#B
�#B
�)B
�#B
�#B
�)B
�)B
�)B
�/B
�/B
�)B
�/B
�)B
�/B
�/B
�)B
�/B
�/B
�5B
�5B
�5B
�5B
�/B
�5B
�/B
�)B
�#B
�)B
�TB
��B
��B
��BoB�B
ŢB
��B
�dB
�Bm�B��B��B��BPB{B"�B.B6FB:^B?}BC�BB�B,B#�B!�B49BdZBjBhsBZBG�B;dBB�B@�B-B�B%BB��B�
BB�B��Bt�BW
BI�B@�B2-BhB  B
�mB
��B
�}B
�B
�{B
p�B
dZB
M�B
)�B
bB
B	��B	�;B	��B	�-B	��B	�oB	y�B	W
B	?}B	uB��B�B��B�qB��B��B��B��B�uB�oB�7B�%B�B�B�B�B�B�B�B�B�%B�%B�1B�PB�VB�bB�uB�uB��B��B�B�'B�9B�FB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�LB�jB�wBÖB��B�B�fB�B�B�B�ZB�
B��B��B��B�
B�
B�
B�
B�B�
B�BB�B�B�B�B�B�B�B�B�B�B�B�B�yB�mB�mB�`B�HB�#B�B�
B�B�B�/B�5B�BB�HB�fB�fB�sB��B	1B	JB	JB	PB	PB	hB	�B	�B	�B	�B	�B	�B	�B	(�B	+B	)�B	(�B	"�B	#�B	2-B	(�B	(�B	0!B	5?B	8RB	>wB	A�B	D�B	B�B	@�B	A�B	B�B	F�B	J�B	K�B	N�B	O�B	R�B	[#B	^5B	_;B	_;B	^5B	_;B	`BB	`BB	`BB	`BB	bNB	ffB	iyB	m�B	n�B	q�B	o�B	n�B	r�B	t�B	w�B	� B	�B	�B	�B	�B	�B	�%B	�B	�B	�%B	�1B	�1B	�=B	�PB	�bB	�hB	�oB	�oB	�oB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�3B	�?B	�LB	�LB	�LB	�LB	�RB	�RB	�XB	�dB	�jB	�qB	�wB	�wB	�}B	��B	��B	��B	��B	B	B	ÖB	ÖB	ĜB	ŢB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�/B	�5B	�BB	�HB	�NB	�fB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B	��B
  B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
+B
1B
DB
DB
DB

=B
DB
DB
DB
DB
DB
DB
DB
DB
JB
JB
PB
PB
PB
PB
PB
PB
VB
\B
\B
bB
bB
bB
bB
hB
hB
hB
hB
hB
hB
oB
oB
oB
oB
oB
uB
uB
uB
{B
2B
%,B
-�2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.28 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005190535                              AO  ARCAADJP                                                                    20181005190535    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005190535  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005190535  QCF$                G�O�G�O�G�O�8000            