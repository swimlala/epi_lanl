CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-11-21T12:59:47Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         (Matthew Alkire, University of Washington      @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7,   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  74   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7t   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     88   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8X   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8x   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           8|   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~            8�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
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
resolution        :�o     �  U�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ]�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  _�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    g�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  i�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  qt   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    yh   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {h   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �\   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �\   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �P   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �    HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �$   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �(   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �,Argo profile    3.1 1.2 19500101000000  20181121125947  20190405100755  5904462 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  5287                            2C  D   APEX                            6529                            072314                          846 @���;�E1   @������0@0�33333�d�O�;dZ1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   A   A   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0ffB8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2�C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb�Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DXfDX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt` Dyl�D�  D�I�D�� D�� D�fD�P D��3D��fD��D�C3D�p D�� D��D�@ Dڙ�D��fD�  D�L�D� D��311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@�G�A��A(��AH��Ah��A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�B(�B
(�B(�B(�B"(�B*(�B2�\B:(�BB(�BJ(�BR(�BZ(�Bb(�Bj(�Br(�Bz(�B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�G�B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{C �=C�=C�=C�=C�=C
�=C�=C�=C�=C�=C�=C�=C�=C�=C�=C�=C �=C"�=C$�=C&�=C(�=C*�=C,�=C.�=C0�=C2��C4�=C6�=C8�=C:�=C<�=C>�=C@�=CB�=CD�=CF�=CH�=CJ�=CL�=CN�=CP�=CR�=CT�=CV�=CX�=CZ�=C\�=C^�=C`�=Cb��Cd�=Cf�=Ch�=Cj�=Cl�=Cn�=Cp�=Cr�=Ct�=Cv�=Cx�=Cz�=C|�=C~�=C�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�ED "�D ��D"�D��D"�D��D"�D��D"�D��D"�D��D"�D��D"�D��D"�D��D	"�D	��D
"�D
��D"�D��D"�D��D"�D��D"�D��D"�D��D"�D��D"�D��D"�D��D"�D��D"�D��D"�D��D"�D��D"�D��D"�D��D"�D��D"�D��D"�D��D"�D��D"�D��D"�D��D"�D��D "�D ��D!"�D!��D""�D"��D#"�D#��D$"�D$��D%"�D%��D&"�D&��D'"�D'��D("�D(��D)"�D)��D*"�D*��D+"�D+��D,"�D,��D-"�D-��D."�D.��D/"�D/��D0"�D0��D1"�D1��D2"�D2��D3"�D3��D4"�D4��D5"�D5��D6"�D6��D7"�D7��D8"�D8��D9"�D9��D:"�D:��D;"�D;��D<"�D<��D="�D=��D>"�D>��D?"�D?��D@"�D@��DA"�DA��DB"�DB��DC"�DC��DD"�DD��DE"�DE��DF"�DF��DG"�DG��DH"�DH��DI"�DI��DJ"�DJ��DK"�DK��DL"�DL��DM"�DM��DN"�DN��DO"�DO��DP"�DP��DQ"�DQ��DR"�DR��DS"�DS��DT"�DT��DU"�DU��DV"�DV��DW"�DW��DX(�DX��DY"�DY��DZ"�DZ��D["�D[��D\"�D\��D]"�D]��D^"�D^��D_"�D_��D`"�D`��Da"�Da��Db"�Db��Dc"�Dc��Dd"�Dd��De"�De��Df"�Df��Dg"�Dg��Dh"�Dh��Di"�Di��Dj"�Dj��Dk"�Dk��Dl"�Dl��Dm"�Dm��Dn"�Dn��Do"�Do��Dp"�Dp��Dq"�Dq��Dr"�Dr��Ds"�Ds��Dt"�Dt��Dy�\D�1HD�Z�D��HD��HD�'�D�aHD��{D���D�D�T{D��HD��HD�*�D�QHDڪ�D�׮D�HD�^D�HD��{11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�?}A�?}A�E�A�C�A�O�A�ZA�^5A�\)A�\)A�^5A�^5A�`BA�`BA�^5A�^5A�`BA�^5A�`BA�`BA�`BA�9XA���A�7LA�`BA�ȴA�&�A�K�A�1AށA�|�A� �A�JA�A�A���Aؕ�Aם�A��Aև+A��TA�ĜA�+A�oA�A�A��A�ĜAѕ�A�5?A�A�t�A�v�A�A��/ȂhA˓uA� �AʓuA���A���A�S�A���A�ĜA���A�Q�A��A�{A���A���A���A�XA�ƨA�hsA�oA��\A�A�&�A���A��TA�M�A���A�$�A���A�\)A�?}A��A��
A��A��yA��A���A��A�ffA���A���A��A��A��A��A���A��A�5?A��A�oA��9A�ffA�%A���A�M�A�$�A�bA�\)A~�+A{�
Ax(�Ar��Ao�TAmoAgG�AdA�Aa;dA\(�AT�ARA�AO��AN~�ALJAH�+AGXAFr�AC�TACAB��AAVA>�9A<��A;��A;�A:ZA7�#A7%A6��A5&�A4M�A2bNA0�9A0M�A-�mA,ĜA,�`A,�yA+�PA+�hA++A*9XA)hsA'+A#�A"n�A"A!�7A!&�AƨA�hA
=An�AA�A�-A��AM�A{A�A�wA%AbNAVA�
Al�AC�A7LA�yA��A�AO�AA�9A=qA��A��AffA�A�wA��A\)AVAG�AĜAdZA
��A	�-A��A�RAbNA�AbA�Az�An�AbA�A�A?}AoA^5AQ�AE�AQ�A^5AE�A��AS�A%A�/A�A��A��A�A �A �\A {@�S�@�5?@�r�@�dZ@�ȴ@�5?@�`B@�V@��j@���@���@�/@� �@�t�@��H@���@�%@�Q�@�V@��@�9X@�t�@�+@���@�5?@�X@�j@��@�
=@�ff@�@陚@���@�l�@�M�@�h@�j@�Z@���@�S�@�\@��#@�7@�hs@�?}@��u@��@�33@�ff@�J@݁@��@� �@ۮ@�t�@�K�@���@ڟ�@�=q@�J@١�@��@���@�%@�%@��@׮@�
=@�M�@ղ-@�p�@�/@���@��/@�1'@Ӆ@��y@���@�`B@д9@Ѓ@�1'@Ϯ@θR@�v�@�M�@�-@��#@�G�@�Ĝ@�Z@�ƨ@˥�@˕�@ˍP@�\)@ʧ�@�^5@�E�@�$�@�@�@Ɂ@��/@�I�@���@�dZ@���@Ɨ�@�v�@�5?@Ų-@š�@ř�@őh@Ł@�?}@�Ĝ@�bN@å�@�+@���@��@§�@�ff@�=q@�{@�@�`B@���@�Z@�A�@�9X@��@�o@�ff@�J@��@�@�hs@���@��9@�j@�9X@�  @��w@���@�S�@���@�-@��^@�X@�bN@�l�@�
=@�=q@��@�p�@��/@��D@�j@�I�@��m@�dZ@�
=@��@��R@�n�@�=q@�@�x�@�V@�z�@�9X@�Z@���@��@���@�=q@���@��^@��-@�%@���@�j@�1'@� �@�b@�  @���@�dZ@�"�@��@��+@�$�@��^@�hs@���@�bN@�1'@�1@�  @��@�ƨ@�l�@�;d@�
=@��!@��+@�ff@�-@��T@�@�X@���@���@�(�@��
@��;@��@��!@�n�@��@��@���@���@���@���@���@��D@�A�@��@���@�|�@�ȴ@�~�@�n�@�@�&�@��/@�bN@�b@��
@��@�ƨ@�ƨ@��@�\)@�
=@�ȴ@�M�@��@���@��7@�G�@���@��`@��/@���@���@�7L@�bN@�|�@�@�@w;d@o|�@dI�@[@N5?@F��@@��@8A�@.ȴ@)X@#��@
=@"�@�h@7L@ff11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�?}A�?}A�E�A�C�A�O�A�ZA�^5A�\)A�\)A�^5A�^5A�`BA�`BA�^5A�^5A�`BA�^5A�`BA�`BA�`BA�9XA���A�7LA�`BA�ȴA�&�A�K�A�1AށA�|�A� �A�JA�A�A���Aؕ�Aם�A��Aև+A��TA�ĜA�+A�oA�A�A��A�ĜAѕ�A�5?A�A�t�A�v�A�A��/ȂhA˓uA� �AʓuA���A���A�S�A���A�ĜA���A�Q�A��A�{A���A���A���A�XA�ƨA�hsA�oA��\A�A�&�A���A��TA�M�A���A�$�A���A�\)A�?}A��A��
A��A��yA��A���A��A�ffA���A���A��A��A��A��A���A��A�5?A��A�oA��9A�ffA�%A���A�M�A�$�A�bA�\)A~�+A{�
Ax(�Ar��Ao�TAmoAgG�AdA�Aa;dA\(�AT�ARA�AO��AN~�ALJAH�+AGXAFr�AC�TACAB��AAVA>�9A<��A;��A;�A:ZA7�#A7%A6��A5&�A4M�A2bNA0�9A0M�A-�mA,ĜA,�`A,�yA+�PA+�hA++A*9XA)hsA'+A#�A"n�A"A!�7A!&�AƨA�hA
=An�AA�A�-A��AM�A{A�A�wA%AbNAVA�
Al�AC�A7LA�yA��A�AO�AA�9A=qA��A��AffA�A�wA��A\)AVAG�AĜAdZA
��A	�-A��A�RAbNA�AbA�Az�An�AbA�A�A?}AoA^5AQ�AE�AQ�A^5AE�A��AS�A%A�/A�A��A��A�A �A �\A {@�S�@�5?@�r�@�dZ@�ȴ@�5?@�`B@�V@��j@���@���@�/@� �@�t�@��H@���@�%@�Q�@�V@��@�9X@�t�@�+@���@�5?@�X@�j@��@�
=@�ff@�@陚@���@�l�@�M�@�h@�j@�Z@���@�S�@�\@��#@�7@�hs@�?}@��u@��@�33@�ff@�J@݁@��@� �@ۮ@�t�@�K�@���@ڟ�@�=q@�J@١�@��@���@�%@�%@��@׮@�
=@�M�@ղ-@�p�@�/@���@��/@�1'@Ӆ@��y@���@�`B@д9@Ѓ@�1'@Ϯ@θR@�v�@�M�@�-@��#@�G�@�Ĝ@�Z@�ƨ@˥�@˕�@ˍP@�\)@ʧ�@�^5@�E�@�$�@�@�@Ɂ@��/@�I�@���@�dZ@���@Ɨ�@�v�@�5?@Ų-@š�@ř�@őh@Ł@�?}@�Ĝ@�bN@å�@�+@���@��@§�@�ff@�=q@�{@�@�`B@���@�Z@�A�@�9X@��@�o@�ff@�J@��@�@�hs@���@��9@�j@�9X@�  @��w@���@�S�@���@�-@��^@�X@�bN@�l�@�
=@�=q@��@�p�@��/@��D@�j@�I�@��m@�dZ@�
=@��@��R@�n�@�=q@�@�x�@�V@�z�@�9X@�Z@���@��@���@�=q@���@��^@��-@�%@���@�j@�1'@� �@�b@�  @���@�dZ@�"�@��@��+@�$�@��^@�hs@���@�bN@�1'@�1@�  @��@�ƨ@�l�@�;d@�
=@��!@��+@�ff@�-@��T@�@�X@���@���@�(�@��
@��;@��@��!@�n�@��@��@���@���@���@���@���@��D@�A�@��@���@�|�@�ȴ@�~�@�n�@�@�&�@��/@�bN@�b@��
@��@�ƨ@�ƨ@��@�\)@�
=@�ȴ@�M�@��@���@��7@�G�@���@��`@��/@���@���@�7L@�bN@�|�@�@�@w;d@o|�@dI�@[@N5?@F��@@��@8A�@.ȴ@)X@#��@
=@"�@�h@7L@ff11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�#B	��B
B
�B
 �B
<jB
B�B
G�B
T�B
w�B
{�B
y�B
u�B
t�B
m�B
n�B
v�B
}�B
�hB
�uB
��B
�wB
��B
�B
�B
��B
��B
��B	7B"�B5?BH�BffB}�B�=B��B�B�9BȴB�BB$�B0!B33B<jB6FB=qB>wBM�BjBn�Bm�Bk�BE�B0!B�BVB1BB��B��B��B  B��B��B��B��B��B�B�BȴB��Br�BO�B)�BhB
��B
�fB
�sB
�ZB
��B
��B
��B
��B
|�B
^5B
P�B
$�B
DB	��B	�`B	��B	�dB	��B	�JB	x�B	T�B	A�B	/B	hB�BB�)B�B��B��B�B�#B�)B�TB�TB�NB�yB�B��B��B��B��B�B�B�mB�TB�TB�#B�B�5B�B�5B�ZB	B	
=B	"�B	&�B	%�B	 �B	�B	�B	�B	.B	2-B	8RB	2-B	6FB	<jB	A�B	M�B	YB	[#B	_;B	e`B	ffB	iyB	s�B	x�B	z�B	� B	�%B	�+B	�+B	�JB	�{B	��B	��B	��B	��B	��B	��B	��B	�-B	�!B	�B	��B	��B	�B	�B	�B	��B	�{B	�VB	�\B	�VB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�!B	�-B	�?B	�LB	�RB	�XB	�qB	�wB	�}B	��B	B	ÖB	ÖB	B	��B	�}B	�jB	�RB	�?B	�3B	�'B	�'B	�-B	�FB	�jB	ĜB	��B	��B	��B	��B	��B	ɺB	ŢB	��B	�wB	��B	��B	B	B	��B	�}B	�}B	�wB	�qB	�qB	�qB	�qB	�jB	�jB	�jB	�jB	�jB	�jB	�jB	�dB	�dB	�dB	�dB	�dB	�dB	�jB	�jB	�qB	�wB	�wB	�wB	�}B	ÖB	ĜB	ĜB	ĜB	ƨB	ǮB	ǮB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�#B	�#B	�)B	�/B	�/B	�/B	�;B	�;B	�;B	�;B	�;B	�;B	�BB	�HB	�TB	�TB	�ZB	�ZB	�ZB	�fB	�fB	�fB	�fB	�fB	�fB	�fB	�fB	�mB	�mB	�sB	�yB	�yB	�yB	�yB	�B	�B	�B	�B	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B	��B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
+B
+B
+B
+B
1B
1B
+B
+B
	7B
	7B
1B
1B
	7B

=B

=B

=B

=B

=B
JB
JB
PB
PB
PB
PB
PB
PB
PB
JB
DB
DB
JB
JB
PB
VB
\B
bB
bB
\B
\B
\B
VB
VB
PB
PB
VB
\B
\B
\B
\B
hB
bB
�B
�B
#�B
'�B
.B
5?B
?}B
F�B
M�B
O�B
R�B
XB
_;B
cTB
gmB
l�B
o�B
u�B
y�B
}�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B	˝B	̥B	˟B	̥B	˞B	˜B	˜B	̤B	˜B	˞B	˜B	˞B	̤B	̥B	̥B	̡B	ͪB	ͫB	ͬB	ͬB	��B	��B
�B
\B
 �B
<AB
BfB
G�B
T�B
w�B
{�B
y�B
u�B
t�B
mgB
nnB
v�B
}�B
�>B
�JB
��B
�OB
��B
�TB
�yB
��B
��B
��B	B"�B5BH�Bf=B}�B�B�_B��B�BȇB�QB�B$�B/�B3B<=B6B=BB>IBM�BjQBnhBmaBkWBEsB/�BpB&B B�B��B��B��B��B��B��B��B��B��B�B��BȃB��Br{BO�B)�B6B
��B
�1B
�>B
�"B
̚B
��B
��B
�YB
|�B
^ B
P�B
$�B
B	��B	�+B	ѶB	�-B	��B	�B	x�B	T�B	ARB	.�B	.B�
B��B��BӽB��B��B��B��B�B�B�B�>B�zB��B��B��B��B�uB�HB�0B�B�B��B��B��B��B��B�B	�B	
 B	"�B	&�B	%�B	 �B	JB	SB	mB	-�B	1�B	8B	1�B	6B	<*B	AJB	M�B	X�B	Z�B	^�B	e"B	f'B	i9B	swB	x�B	z�B	�B	��B	��B	��B	�
B	�>B	�LB	�fB	�zB	�~B	�yB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�;B	�B	�B	�B	�.B	�OB	�RB	�FB	�wB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�1B	�6B	�:B	�CB	�MB	�UB	�SB	�LB	�GB	�=B	�&B	�B	��B	��B	��B	��B	��B	�B	�*B	�ZB	ʁB	˄B	ϝB	ϜB	˂B	�wB	�`B	�@B	�3B	�?B	�BB	�LB	�NB	�HB	�:B	�8B	�1B	�.B	�-B	�.B	�.B	�&B	�%B	�&B	�$B	�&B	�(B	�%B	�!B	� B	�"B	�#B	�!B	�"B	�"B	�%B	�.B	�5B	�3B	�3B	�9B	�RB	�ZB	�VB	�XB	�eB	�kB	�jB	�iB	�uB	͑B	ΓB	ϛB	ϙB	ΕB	ϚB	ϜB	ϚB	РB	ТB	ѨB	ԺB	վB	վB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	� B	�"B	�"B	� B	�!B	�"B	�#B	�!B	�)B	�(B	�.B	�5B	�6B	�5B	�5B	�9B	�<B	�9B	�:B	�5B	�4B	�=B	�:B	�=B	�FB	�DB	�GB	�GB	�EB	�LB	�HB	�LB	�LB	�LB	�SB	�RB	�RB	�SB	�UB	�YB	�`B	�`B	�^B	�aB	�cB	�hB	�kB	�iB	�hB	�qB	�sB	�rB	�wB	�B	�{B	��B	�xB	�xB	�|B	�xB	�vB	�xB	�pB	�xB	�pB	�tB	�sB	�~B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
	�B
	�B
	�B
	�B
	�B
B
B
B
	B
B

B
B
B

B
B

�B

�B
B
B
	B
B
B
B
B
B
B
B
B
B
	B
	B
B
B
B
B
B
!B
B
;B
oB
#�B
'�B
-�B
4�B
?5B
FaB
M�B
O�B
R�B
W�B
^�B
cB
g'B
lEB
oWB
uB
y�B
}�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.54 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                                                                                                             201904051007552019040510075520190405100755  AO  ARCAADJP                                                                    20181121125947    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181121125947  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181121125947  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20190405100755  IP                  G�O�G�O�G�O�                