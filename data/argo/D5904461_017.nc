CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-02-26T22:13:37Z AOML 3.0 creation; 2016-08-07T21:36:30Z UW 3.1 conversion     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7    PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7X   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8<   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8\   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~       axis      T           8`   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8h   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~            8l   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8t   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8|   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    K�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  U|   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ]p   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  _p   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    gd   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  id   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  qX   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    yL   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {L   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �@   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �@   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �4   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �d   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �d   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �d   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �d   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20150226221337  20160807143630  5904461 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               A   AO  5286_8897_017                   2C  D   APEX                            6531                            072314                          846 @�'nT�@1   @�'n���@1y������c�r� Ĝ1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   B   B   @�  @�  A   A   A@  A`  A�  A�  A���A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BHffBP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�ffB�33B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	y�D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dty�Dy�D�fD�6fD�� D��3D�	�D�0 D�� D���D�3D�VfD�y�D��fD��D�S3Dڙ�D��3D� D�9�D�fD��f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��A ��A ��A@��A`��A�z�A�z�A�G�A�z�A�z�A�z�A�z�A�z�B =qB=qB=qB=qB =qB(=qB0=qB8=qB@=qBH��BP=qBX=qB`=qBh=qBp=qBx=qB��B��B��B��B��B��B��B�Q�B��B��B��B��B��B��B��B��B�Q�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��C \C\C\C\C\C
\C\C\C\C\C\C\C\C\C\C\C \C"\C$\C&\C(\C*\C,\C.\C0\C2\C4\C6\C8\C:\C<\C>\C@\CB\CD\CF\CH\CJ\CL\CN\CP\CR\CT\CV\CX\CZ\C\\C^\C`\Cb\Cd\Cf\Ch\Cj\Cl\Cn\Cp\Cr\Ct\Cv\Cx\Cz\C|\C~\C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C���C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	}qD
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D�=D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt}qDy�D�RD�8RD���D��D��D�1�D���D���D�D�XRD�{�D��RD��D�UDڛ�D��D��D�;�D�RD��R11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A���A�ȴA�ȴA���A���A���A���A���A���A���A���A���A���A���A��
A��#A��HA��A��A� �A�^5A�`BA�ZA�XA�XA�VA�XA�bNAՇ+A՗�AՃA�~�A�x�AՃAՓuA՛�AոRA��`A��TA���Aգ�A�K�A�9XA�AѮA�-A�A®A��A��RA��A�{A�oA�\)A��A�t�A��A���A�x�A�  A��PA��A���A��7A�&�A�p�A�E�A�oA�I�A��^A�M�A���A���A�+A��TA���A�JA�r�A��hA�{A��DA��RA�ZA��
A�"�A�JA��\A�`BA���A��7A��FA��TA��wA�{A��A��A��A��A��`A}�;AwhsAt�RAoK�AfJA_�A[��AU�PAQ��AM?}AKx�AH�AD�\AA&�A@  A>v�A<^5A:�A9��A7\)A6=qA5A4�+A1��A/"�A.�A-l�A,��A,v�A,v�A,Q�A,JA+��A*E�A)VA'�^A%O�A#dZA#�A"jA!?}A �\A�hAoAA�AS�A��A�A7LA�HAn�A�A|�A�A�hA%AȴA��Az�A=qA{Ap�A �A|�A/A�A-A��AȴAK�A��A7LAȴAƨA�7A`BAA��A"�A
��A
ĜA
��A
^5A
$�A	�#A	A	%A�Al�AbNA|�AXAr�A�yA ��@�hs@���@�n�@���@�7L@�9X@���@�^5@�M�@�-@��@�&�@�ƨ@@��#@�?}@�@��@�t�@�
=@�^5@��@�(�@��@�&�@��@�^5@���@�Q�@�@�V@ܴ9@�I�@�b@��;@ە�@ڟ�@���@ٺ^@ى7@���@؃@�b@�;d@���@�^5@��/@��@�K�@�~�@�$�@���@�7L@��@Ь@Ѓ@�1'@Ͼw@Ώ\@�X@ˮ@��@���@�~�@��@�7L@ȃ@ǍP@�
=@Ƨ�@�J@�hs@�V@�j@��
@�
=@���@¸R@�v�@�@��7@�`B@�V@�?}@���@��u@��P@��@���@���@��@�S�@�t�@�|�@���@�S�@�;d@�v�@�5?@��@�&�@���@�1@�j@��D@�l�@�33@��y@�@��@���@���@�/@��@���@���@��D@�j@�Q�@�ƨ@�|�@�t�@�l�@�K�@��@��y@�ȴ@���@�E�@�{@��h@�/@�V@���@��/@��9@��D@�1@�K�@�o@�C�@�+@���@���@�ȴ@���@�^5@�J@��7@�`B@�X@��@��@��
@�t�@�l�@�;d@�o@���@�v�@�5?@��@��T@�v�@�5?@�`B@���@�Z@�A�@�1'@�(�@�A�@��;@�S�@�
=@���@���@�@���@�X@�X@�?}@�?}@�/@��9@�r�@�A�@�b@�dZ@�o@�@���@���@��@��D@�ƨ@��;@��m@�@��R@�ȴ@�5?@��@��@�r�@�t�@��@���@�^5@�J@���@�`B@�G�@���@��D@�I�@���@�dZ@�+@���@��H@�ȴ@�^5@�$�@��@��#@���@��`@���@�z�@�r�@�j@�r�@�r�@�Z@���@���@��@�S�@�@��+@�E�@�$�@�@��-@���@�X@�&�@��@���@�Q�@�b@��;@��w@�\)@��y@���@�E�@���@�`B@�%@�Ĝ@�j@� �@�b@�  @��
@�|�@�33@��@��R@��+@�M�@�5?@�$�@�@��@��-@��h@�`B@�/@���@�A�@���@���@���@��P@�l�@�\)@�C�@�+@�@�ȴ@�9X@��@yx�@r�@ix�@`1'@Zn�@T1@NE�@C�m@:��@2��@,z�@&v�@!&�@��@Q�@j@K�@�@�u11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   A���A�ȴA�ȴA���A���A���A���A���A���A���A���A���A���A���A��
A��#A��HA��A��A� �A�^5A�`BA�ZA�XA�XA�VA�XA�bNAՇ+A՗�AՃA�~�A�x�AՃAՓuA՛�AոRA��`A��TA���Aգ�A�K�A�9XA�AѮA�-A�A®A��A��RA��A�{A�oA�\)A��A�t�A��A���A�x�A�  A��PA��A���A��7A�&�A�p�A�E�A�oA�I�A��^A�M�A���A���A�+A��TA���A�JA�r�A��hA�{A��DA��RA�ZA��
A�"�A�JA��\A�`BA���A��7A��FA��TA��wA�{A��A��A��A��A��`A}�;AwhsAt�RAoK�AfJA_�A[��AU�PAQ��AM?}AKx�AH�AD�\AA&�A@  A>v�A<^5A:�A9��A7\)A6=qA5A4�+A1��A/"�A.�A-l�A,��A,v�A,v�A,Q�A,JA+��A*E�A)VA'�^A%O�A#dZA#�A"jA!?}A �\A�hAoAA�AS�A��A�A7LA�HAn�A�A|�A�A�hA%AȴA��Az�A=qA{Ap�A �A|�A/A�A-A��AȴAK�A��A7LAȴAƨA�7A`BAA��A"�A
��A
ĜA
��A
^5A
$�A	�#A	A	%A�Al�AbNA|�AXAr�A�yA ��@�hs@���@�n�@���@�7L@�9X@���@�^5@�M�@�-@��@�&�@�ƨ@@��#@�?}@�@��@�t�@�
=@�^5@��@�(�@��@�&�@��@�^5@���@�Q�@�@�V@ܴ9@�I�@�b@��;@ە�@ڟ�@���@ٺ^@ى7@���@؃@�b@�;d@���@�^5@��/@��@�K�@�~�@�$�@���@�7L@��@Ь@Ѓ@�1'@Ͼw@Ώ\@�X@ˮ@��@���@�~�@��@�7L@ȃ@ǍP@�
=@Ƨ�@�J@�hs@�V@�j@��
@�
=@���@¸R@�v�@�@��7@�`B@�V@�?}@���@��u@��P@��@���@���@��@�S�@�t�@�|�@���@�S�@�;d@�v�@�5?@��@�&�@���@�1@�j@��D@�l�@�33@��y@�@��@���@���@�/@��@���@���@��D@�j@�Q�@�ƨ@�|�@�t�@�l�@�K�@��@��y@�ȴ@���@�E�@�{@��h@�/@�V@���@��/@��9@��D@�1@�K�@�o@�C�@�+@���@���@�ȴ@���@�^5@�J@��7@�`B@�X@��@��@��
@�t�@�l�@�;d@�o@���@�v�@�5?@��@��T@�v�@�5?@�`B@���@�Z@�A�@�1'@�(�@�A�@��;@�S�@�
=@���@���@�@���@�X@�X@�?}@�?}@�/@��9@�r�@�A�@�b@�dZ@�o@�@���@���@��@��D@�ƨ@��;@��m@�@��R@�ȴ@�5?@��@��@�r�@�t�@��@���@�^5@�J@���@�`B@�G�@���@��D@�I�@���@�dZ@�+@���@��H@�ȴ@�^5@�$�@��@��#@���@��`@���@�z�@�r�@�j@�r�@�r�@�Z@���@���@��@�S�@�@��+@�E�@�$�@�@��-@���@�X@�&�@��@���@�Q�@�b@��;@��w@�\)@��y@���@�E�@���@�`B@�%@�Ĝ@�j@� �@�b@�  @��
@�|�@�33@��@��R@��+@�M�@�5?@�$�@�@��@��-@��h@�`B@�/@���@�A�@���@���@���@��P@�l�@�\)@�C�@�+@�G�O�@�9X@��@yx�@r�@ix�@`1'@Zn�@T1@NE�@C�m@:��@2��@,z�@&v�@!&�@��@Q�@j@K�@�@�u11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
T�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
S�B
S�B
T�B
T�B
S�B
T�B
VB
XB
\)B
^5B
q�B
�\B
�{B
��B
��B
��B
��B
��B
��B
��B
��BoBuB�B �B,B1'BF�Br�B�B�B�1B�VB�hB�uB�\B��B�}B�#B�B+B-B�B
=BoBhBhB-B9XBD�BM�BffBffBjBl�BgmBW
BE�B>wB<jB<jB>wBA�BD�BI�BL�BI�B=qB,B%B�sB�;B��BȴB�}B�'B�%B\)B>wB6FB/B8RBG�BA�B
��B
�B
�hB
�B
_;B
 �B	�B	B	�B	�B	G�B	"�B	
=B�yB�BȴB��B�^B�3B�B��B��B��B��B��B�{B�{B��B��B��B�!B�9B�9B�?B�FB�?B�FB�FB�LB�^B�qB�qB��BǮBȴBɺB��B��B�#B�#B�/B�5B�5B�5B�5B�5B�;B�;B�;B�BB�HB�mB�B�B�B�B�B�B�B�B�B�B��B��B��B	B	VB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	 �B	�B	�B	"�B	�B	�B	+B�B�B�B�yB�sB�`B�sB�B��B	B��B�B�B�B�B��B��B��B��B	  B	B	+B	
=B	bB	hB	bB	oB	oB	{B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	 �B	 �B	 �B	$�B	'�B	,B	.B	.B	0!B	49B	49B	5?B	6FB	7LB	8RB	8RB	:^B	:^B	;dB	;dB	=qB	@�B	B�B	D�B	E�B	F�B	H�B	L�B	M�B	P�B	R�B	T�B	YB	\)B	^5B	^5B	aHB	cTB	cTB	e`B	k�B	k�B	k�B	jB	l�B	o�B	o�B	p�B	r�B	t�B	v�B	{�B	|�B	~�B	~�B	� B	�B	�+B	�%B	�%B	�7B	�JB	�JB	�DB	�VB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�'B	�3B	�9B	�?B	�?B	�?B	�?B	�9B	�3B	�-B	�9B	�LB	�RB	�RB	�RB	�RB	�XB	�dB	�wB	�wB	�}B	�}B	��B	��B	ÖB	ÖB	ĜB	ŢB	ŢB	ŢB	ŢB	ŢB	ƨB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�B	��B	�
B	�B	�)B	�/B	�HB	�HB	�NB	�ZB	�ZB	�ZB	�ZB	�ZB	�ZB	�fB	�mB	�fB	�fB	�fB	�`B	�mB	�sB	�sB	�mB	�yB	�sB	�mB	�mB	�mB	�`B	�fB	�mB	�sB	�yB	�yB	�yB	�yB	�sB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
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
%B
%B
%B
+B
+B
	7B
\B
{B
�B
�B
'�B
,B
1'B
6FB
:^B
C�B
M�B
T�B
YB
^5B
cTB
ffB
jB
n�B
s�B
x�B
z�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   B
UB
UB
U B
U B
U B
U B
U B
T�B
UB
S�B
S�B
UB
T�B
S�B
UB
VB
XB
\(B
^8B
q�B
�\B
�|B
��B
��B
��B
��B
��B
��B
��B
��BnBqB�B �B,B1"BF�Br�B�B�B�.B�PB�aB�oB�XB��B�xB�B�B&B-B�B
8BoBeBeB-
B9RBD�BM�BfaBfaBj~Bl�BghBWBE�B>pB<eB<eB>rBA�BD�BI�BL�BI�B=lB,BB�mB�5B��BȰB�uB�#B�B\&B>vB6AB/B8OBG�BA�B
��B
�B
�fB
�B
_6B
 �B	�B	B	�B	�B	G�B	"�B	
DB�B�#BȿB��B�lB�@B�B��B��B��B��B��B��B��B��B��B��B�1B�EB�DB�JB�TB�NB�RB�RB�XB�kB�|B�|B��BǸB��B��B��B��B�.B�-B�8B�?B�@B�AB�AB�AB�GB�EB�FB�MB�RB�vB�B�B�B�B�B�B�B�B��B��B��B��B��B	B	\B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	 �B	�B	�B	"�B	�B	�B	4B�B�B�B�B�|B�hB�|B�B��B	B��B�B�B�B�B��B��B��B�B	 B	B	3B	
CB	lB	pB	kB	vB	wB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	 �B	 �B	 �B	$�B	'�B	,B	.B	.B	0(B	4@B	4>B	5EB	6IB	7QB	8WB	8YB	:cB	:eB	;jB	;iB	=wB	@�B	B�B	D�B	E�B	F�B	H�B	L�B	M�B	P�B	R�B	UB	YB	\,B	^:B	^8B	aMB	cWB	cXB	eeB	k�B	k�B	k�B	j�B	l�B	o�B	o�B	p�B	r�B	t�B	v�B	{�B	|�B	~�B	~�B	�B	�B	�-B	�'B	�(B	�8B	�LB	�KB	�GB	�XB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�(B	�3B	�;B	�?B	�?B	�@B	�@B	�<B	�4B	�-B	�:B	�LB	�SB	�SB	�RB	�TB	�YB	�dB	�wB	�xB	�|B	�|B	��B	��B	×B	ÙB	ĝB	ŢB	šB	ŤB	ţB	ŤB	ƧB	ȵB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�	B	�B	��B	�	B	�B	�*B	�,B	�FB	�EB	�MB	�YB	�XB	�YB	�YB	�XB	�YB	�eB	�mB	�gB	�dB	�eB	�_B	�lB	�sB	�tB	�nB	�yB	�uB	�mB	�oB	�mB	�`B	�eB	�nB	�rB	�zB	�yB	�yB	�zB	�tB	�uB	�sB	�zB	�~B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B

B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
#B
#B
$B
$B
$B
#B
(B
+G�O�B
XB
zB
�B
�B
'�B
,B
1'B
6DB
:\B
C�B
M�B
T�B
YB
^1B
cPB
fdB
j|B
n�B
s�B
x�B
z�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.06 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071436302016080714363020160807143630  AO  ARCAADJP                                                                    20150226221337    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150226221337  QCP$                G�O�G�O�G�O�DFB5E           AO  ARGQQCPL                                                                    20150226221337  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807143630  IP                  G�O�G�O�G�O�                