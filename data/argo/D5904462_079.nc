CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-10-04T09:16:38Z AOML 3.0 creation; 2016-08-07T21:51:22Z UW 3.1 conversion     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20151004091638  20160807145122  5904462 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               OA   AO  5287_9017_079                   2C  D   APEX                            6529                            072314                          846 @�tC��S�1   @�tD335"@/�^5?|��d�&�x��1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    OA   B   B   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�33B   B  B  B  B   B(  B0  B8  B@ffBH  BP  BX  B`  Bh  Bp  Bx  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B���B�  B�  B�  B�  B�  B�  B�  B�  C   C�C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C�fC"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP�CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DIfDI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt��Dy��D�  D�L�D�y�D��3D�#3D�L�D��3D�� D�� D�` D���D�� D�  D�9�Dډ�D��3D�fD�FfD� 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��A��A(��AH��Ah��A�z�A�z�A�z�A�z�A�z�A�z�A�z�A�B=qB
=qB=qB=qB"=qB*=qB2=qB:=qBB��BJ=qBR=qBZ=qBb=qBj=qBr=qBz=qB��B�Q�B��B��B��B��B��B��B��B��B�Q�B��B��B��B��B��B��B��B��B��B��B�Q�B��B��B��B��B��B��B��B��B��B��C �\C��C�\C�\C�\C
�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C u�C"�\C$�\C&�\C(�\C*�\C,�\C.�\C0�\C2�\C4�\C6�\C8�\C:�\C<�\C>�\C@�\CB�\CD�\CF�\CH�\CJ�\CL�\CN�\CP��CR�\CT�\CV�\CX�\CZ�\C\�\C^�\C`�\Cb�\Cd�\Cf�\Ch�\Cj�\Cl�\Cn�\Cp�\Cr�\Ct�\Cv�\Cx�\Cz�\C|�\C~�\C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�T{C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�D #�D ��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D	#�D	��D
#�D
��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D�=D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D #�D ��D!#�D!��D"#�D"��D##�D#��D$#�D$��D%#�D%��D&#�D&��D'#�D'��D(#�D(��D)#�D)��D*#�D*��D+#�D+��D,#�D,��D-#�D-��D.#�D.��D/#�D/��D0#�D0��D1#�D1��D2#�D2��D3#�D3��D4#�D4��D5#�D5��D6#�D6��D7#�D7��D8#�D8��D9#�D9��D:#�D:��D;#�D;��D<#�D<��D=#�D=��D>#�D>��D?#�D?��D@#�D@��DA#�DA��DB#�DB��DC#�DC��DD#�DD��DE#�DE��DF#�DF��DG#�DG��DH#�DH��DI*=DI��DJ#�DJ��DK#�DK��DL#�DL��DM#�DM��DN#�DN��DO#�DO��DP#�DP��DQ#�DQ��DR#�DR��DS#�DS��DT#�DT��DU#�DU��DV#�DV��DW#�DW��DX#�DX��DY#�DY��DZ#�DZ��D[#�D[��D\#�D\��D]#�D]��D^#�D^��D_#�D_��D`#�D`��Da#�Da��Db#�Db��Dc#�Dc��Dd#�Dd��De#�De��Df#�Df��Dg#�Dg��Dh#�Dh��Di#�Di��Dj#�Dj��Dk#�Dk��Dl#�Dl��Dm#�Dm��Dn#�Dn��Do#�Do��Dp#�Dp��Dq#�Dq��Dr#�Dr��Ds#�Ds��Dt#�Dt��Du�DyФD��D�^�D���D��D�5D�^�D��D���D��D�q�D���D���D��D�K�Dڛ�D��D�(RD�XRD��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A���A���A���A���A���A���A���A���A���A���A���A��A��
A��A��
A��
A���A�A���A�ȴA���A��A���A�wA��A�uA�E�A�1A�/A��A�G�A�M�A�VAھwA��A�Q�A�A�A���A��
A�33A��
A���A�5?AсA���A�bNAύPA�A̾wA�?}A�G�A�jA�I�A�&�A�%A���A�1'A���A�^5A��#A�n�A��^A�=qA��A�9XA���A���A�K�A�XA�v�A�A�1A��\A�ĜA���A���A���A��A�ȴA�33A�ffA��!A�hsA���A�XA�|�A�+A��RA��/A�ZA��#A��A�1'A�1A���A�|�A�?}A�ĜA��PA���A�t�A��FA�9XA���A��DA�TA{`BAvVAq��Ao�An�DAg�Ad�Ab��AaoA]�AWVAR�yAQVAN��AMoAJ1'AG�-AE��AD�+A@�A=ƨA;K�A8A6�/A5��A3��A2~�A1�7A/oA.�A-��A,�uA+�-A*A�A'�mA&�A%�-A"��A"r�A!�hA jAƨA/A��A  AbNAz�A�hA�A�uAI�A5?A��A1'A�A�Ax�A�9A��A�A�PA��A1A�#A��AA�A{A|�AXA+A
��A
�jA
ffA
$�A	�wAffA9XA��AK�A?}AK�A�A�;A��A�^A��A&�A ��@�dZ@��^@��@��
@�~�@�bN@��@���@�{@�j@�t�@�X@��`@��@�  @�\)@��-@�j@��@�C�@��H@�n�@�E�@���@�hs@� �@�
=@�ȴ@�v�@�J@�7@��@噚@�E�@��
@��@�P@��m@�I�@��@�!@�v�@��@�C�@��@���@ߝ�@�n�@ް!@�"�@�C�@��T@���@��@ٲ-@أ�@ش9@�  @�ƨ@���@��m@��@�j@�r�@�r�@�o@�n�@�J@� �@ҟ�@�|�@҇+@�+@�@�@��@��@��@υ@�dZ@�
=@�n�@��#@�?}@���@�j@�b@�|�@ʧ�@ʏ\@ʗ�@ʗ�@�ff@ə�@���@�z�@�b@���@�dZ@�C�@��@Ƈ+@�~�@�E�@�{@���@�V@�(�@öF@�t�@�\)@�
=@§�@�v�@�ff@�ff@�^5@�ff@�ff@�M�@�M�@�ff@�M�@�@��T@���@��-@�x�@�X@�/@��`@��@��D@��@�|�@��H@�^5@��@��h@�`B@��@�b@�l�@���@���@�l�@�"�@��y@���@���@��+@�n�@�E�@��#@�X@�&�@���@�Q�@��@�|�@�t�@�33@���@�~�@�=q@���@���@���@��h@�x�@�/@��@�A�@�1@��F@�dZ@�t�@�33@���@�{@��@�$�@��@��-@�@��@�hs@�7L@��/@���@��D@��@���@�+@��@��@�hs@�V@���@��@�|�@�S�@�;d@�ȴ@�-@��^@���@�`B@��/@��u@�(�@�A�@�Q�@�ƨ@�dZ@�\)@�\)@�|�@�o@��R@���@�V@��^@�&�@��/@��j@���@�Q�@� �@���@�ƨ@�S�@�v�@�5?@��@�@��h@�7L@�r�@�j@�1'@�  @���@���@�t�@�@���@�~�@��@���@���@�G�@��`@��9@�Q�@��w@���@��P@��@�|�@�dZ@�+@�ff@���@�`B@�G�@��@��@��9@���@�Z@�1'@�(�@� �@�b@��w@�dZ@�C�@��R@�^5@��@��h@�p�@�G�@�%@���@�z�@�1'@�1@���@���@�K�@���@���@��P@�/@�5?@v@k��@a��@W|�@Q��@KdZ@Bn�@>{@8Q�@3@+C�@'l�@"^5@��@$�@x�@�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111   A���A���A���A���A���A���A���A���A���A���A���A��A��
A��A��
A��
A���A�A���A�ȴA���A��A���A�wA��A�uA�E�A�1A�/A��A�G�A�M�A�VAھwA��A�Q�A�A�A���A��
A�33A��
A���A�5?AсA���A�bNAύPA�A̾wA�?}A�G�A�jA�I�A�&�A�%A���A�1'A���A�^5A��#A�n�A��^A�=qA��A�9XA���A���A�K�A�XA�v�A�A�1A��\A�ĜA���A���A���A��A�ȴA�33A�ffA��!A�hsA���A�XA�|�A�+A��RA��/A�ZA��#A��A�1'A�1A���A�|�A�?}A�ĜA��PA���A�t�A��FA�9XA���A��DA�TA{`BAvVAq��Ao�An�DAg�Ad�Ab��AaoA]�AWVAR�yAQVAN��AMoAJ1'AG�-AE��AD�+A@�A=ƨA;K�A8A6�/A5��A3��A2~�A1�7A/oA.�A-��A,�uA+�-A*A�A'�mA&�A%�-A"��A"r�A!�hA jAƨA/A��A  AbNAz�A�hA�A�uAI�A5?A��A1'A�A�Ax�A�9A��A�A�PA��A1A�#A��AA�A{A|�AXA+A
��A
�jA
ffA
$�A	�wAffA9XA��AK�A?}AK�A�A�;A��A�^A��A&�A ��@�dZ@��^@��@��
@�~�@�bN@��@���@�{@�j@�t�@�X@��`@��@�  @�\)@��-@�j@��@�C�@��H@�n�@�E�@���@�hs@� �@�
=@�ȴ@�v�@�J@�7@��@噚@�E�@��
@��@�P@��m@�I�@��@�!@�v�@��@�C�@��@���@ߝ�@�n�@ް!@�"�@�C�@��T@���@��@ٲ-@أ�@ش9@�  @�ƨ@���@��m@��@�j@�r�@�r�@�o@�n�@�J@� �@ҟ�@�|�@҇+@�+@�@�@��@��@��@υ@�dZ@�
=@�n�@��#@�?}@���@�j@�b@�|�@ʧ�@ʏ\@ʗ�@ʗ�@�ff@ə�@���@�z�@�b@���@�dZ@�C�@��@Ƈ+@�~�@�E�@�{@���@�V@�(�@öF@�t�@�\)@�
=@§�@�v�@�ff@�ff@�^5@�ff@�ff@�M�@�M�@�ff@�M�@�@��T@���@��-@�x�@�X@�/@��`@��@��D@��@�|�@��H@�^5@��@��h@�`B@��@�b@�l�@���@���@�l�@�"�@��y@���@���@��+@�n�@�E�@��#@�X@�&�@���@�Q�@��@�|�@�t�@�33@���@�~�@�=q@���@���@���@��h@�x�@�/@��@�A�@�1@��F@�dZ@�t�@�33@���@�{@��@�$�@��@��-@�@��@�hs@�7L@��/@���@��D@��@���@�+@��@��@�hs@�V@���@��@�|�@�S�@�;d@�ȴ@�-@��^@���@�`B@��/@��u@�(�@�A�@�Q�@�ƨ@�dZ@�\)@�\)@�|�@�o@��R@���@�V@��^@�&�@��/@��j@���@�Q�@� �@���@�ƨ@�S�@�v�@�5?@��@�@��h@�7L@�r�@�j@�1'@�  @���@���@�t�@�@���@�~�@��@���@���@�G�@��`@��9@�Q�@��w@���@��P@��@�|�@�dZ@�+@�ff@���@�`B@�G�@��@��@��9@���@�Z@�1'@�(�@� �@�b@��w@�dZ@�C�@��R@�^5@��@��h@�p�@�G�@�%@���@�z�@�1'@�1@���@���@�K�@���G�O�@��P@�/@�5?@v@k��@a��@W|�@Q��@KdZ@Bn�@>{@8Q�@3@+C�@'l�@"^5@��@$�@x�@�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
uB
uB
uB
oB
hB
hB
hB
hB
oB
oB
oB
oB
oB
oB
oB
oB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
VB
JB
JB
JB

=B
1B
hB
�B
+B
8RB
VB
�B
�VB
�uB
�bB
��B
�5B
��B"�B=qBffB��B��B�BPB�B�B�B-B@�BbNBs�Bv�By�B|�B�B�B�oB��B��B��B��B��B��B��B��B�+B� Bx�Bo�Bl�BffBYB?}B#�B+B�B�B�)B��Bs�B\)BW
B[#B^5B\)BP�BA�B/B5?B=qB�B
��B
�B
ǮB
�^B
��B
�B
bNB
F�B
&�B
B	�/B	ɺB	�jB	�{B	�B	s�B	gmB	T�B	5?B	#�B	�B	uB	DB	  B��B�B�B�5B�B��BɺBƨBB�wB�dB�LB�9B�'B�B�B��B��B��B��B��B��B��B��B��B��B�{B�{B�oB�\B�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�-B�dB�wBĜB��B��B�B�)B�BB�NB�ZB�yB��B��B��B	B	JB	PB	\B	�B	 �B	�B	bB	uB	VB��B��B	B	B	B��B	  B��B	B	B	%B	B	B	+B	
=B	
=B	
=B	DB	DB	JB	PB	VB	bB	oB	�B	�B	�B	�B	�B	�B	�B	"�B	%�B	)�B	0!B	9XB	P�B	YB	]/B	e`B	m�B	q�B	r�B	p�B	m�B	jB	hsB	dZB	_;B	\)B	aHB	ffB	hsB	hsB	l�B	s�B	e`B	`BB	bNB	dZB	dZB	e`B	hsB	n�B	s�B	u�B	w�B	t�B	w�B	w�B	s�B	s�B	�B	�B	�=B	�JB	�VB	�\B	�bB	�\B	�bB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�!B	�!B	�!B	�B	�B	�B	�'B	�-B	�9B	�LB	�RB	�RB	�XB	�dB	�jB	�qB	�wB	��B	ÖB	ƨB	ǮB	ȴB	ɺB	ɺB	ɺB	ɺB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�#B	�#B	�#B	�#B	�#B	�#B	�#B	�#B	�)B	�/B	�/B	�/B	�/B	�;B	�NB	�NB	�HB	�BB	�HB	�TB	�ZB	�TB	�fB	�yB	�yB	�yB	�yB	�yB	�yB	�yB	�sB	�yB	�yB	�sB	�mB	�sB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
%B
%B
%B
%B
+B
	7B
	7B
	7B
	7B
	7B

=B

=B
DB
JB
JB
JB
PB
VB
\B
\B
bB
bB
hB
oB
uB
uB
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
%�B
.B
5?B
<jB
E�B
J�B
M�B
Q�B
YB
]/B
cTB
ffB
jB
q�B
u�B
y�B
~�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111   B
SB
TB
SB
LB
FB
HB
DB
BB
LB
LB
LB
NB
LB
NB
LB
NB
TB
XB
`B
sB
�B
�B
�B
�B
kB
^B
6B
,B
+B
(B

B
B
DB
�B
*�B
8.B
U�B
��B
�/B
�NB
�9B
�mB
�B
��B"�B=HBf:B��BͧB��B"B{B�B�B,�B@TBb Bs�Bv�By�B|�B��B��B�@B�iB�~B�vB��B��B�mB�iB�TB��B�Bx�BomBlWBf6BX�B?JB#�B�B�UB�JB��B�kBs�B[�BV�BZ�B]�B[�BP�BAVB.�B5
B=>BvB
�B
��B
�zB
�)B
��B
��B
bB
FrB
&�B
 �B	��B	ɊB	�=B	�KB	��B	s�B	g>B	T�B	5B	#�B	zB	HB	B��B��B�}B�ZB�	B��B��BɏB�|B�eB�LB�8B�!B�B��B��B��B��B��B��B��B�yB�tB�`B�[B�SB�QB�NB�OB�BB�1B�LB�ZB�`B�gB�fB�gB�mB��B��B��B��B��B��B��B��B��B��B�3B�GB�lBʏBϮB��B��B�B�B�(B�GB�B��B��B	�B	B	B	&B	gB	 �B	{B	+B	@B	!B��B��B	�B	�B	 �B��B��B��B	 �B	�B	�B	�B	�B	�B	
B	
B	
B	B	B	B	B	B	+B	6B	IB	TB	UB	gB	uB	rB	�B	"�B	%�B	)�B	/�B	9B	P�B	X�B	\�B	e%B	mXB	qoB	ruB	pjB	mUB	jDB	h7B	d B	_ B	[�B	aB	f)B	h8B	h8B	lPB	s|B	e$B	`B	bB	d B	d!B	e%B	h;B	n\B	szB	u�B	w�B	t�B	w�B	w�B	syB	syB	��B	��B	�B	�B	�B	�B	�%B	�B	�&B	�9B	�DB	�AB	�TB	�\B	�nB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�&B	�,B	�3B	�9B	�EB	�UB	�hB	�nB	�uB	�~B	�|B	�|B	�{B	�{B	�|B	ʃB	ʃB	ʅB	̍B	ΚB	ϞB	ΛB	͕B	̌B	̍B	˅B	ʃB	�{B	ХB	ѮB	ЦB	ѫB	ѬB	ԽB	ԾB	ԾB	ԻB	ԽB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�
B	�B	�B	�B	�&B	�8B	�:B	�9B	�8B	�6B	�7B	�8B	�3B	�8B	�8B	�3B	�,B	�4B	�1B	�1B	�9B	�KB	�LB	�RB	�PB	�VB	�UB	�]B	�WB	�[B	�]B	�jB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
	�B
	�B
B
	B
B
B
B
B
B
B
 B
"B
$B
+B
4B
1B
6B
9B
6B
<B
DB
EB
BB
EB
?G�O�B
DB
QB
fB
%�B
-�B
4�B
<$B
E]B
J}B
M�B
Q�B
X�B
\�B
cB
f$B
j;B
qhB
u~B
y�B
~�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.56 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071451222016080714512220160807145122  AO  ARCAADJP                                                                    20151004091638    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20151004091638  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20151004091638  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807145122  IP                  G�O�G�O�G�O�                