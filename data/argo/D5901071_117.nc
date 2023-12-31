CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-13T22:40:23Z UW 3.1 conversion   
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  78   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7x   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8<   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8\   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~       axis      T           8`   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8h   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~            8l   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8t   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8|   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  K|   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  Mx   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  Uh   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ]X   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  _T   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  gD   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  i@   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  q0   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  y    PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  {   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �(   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �(   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �(   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �(   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �T   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �X   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �\   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �`   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �d   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   units         decibar    
_FillValue        G�O�        ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    units         decibar    
_FillValue        G�O�        ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  5901071 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               uA   AO  20111130141828  20190522121826  1727_5046_117                   2C  D   APEX                            2143                            040306                          846 @Ԭ�E�1   @Ԭ�8�@@6�+I��c�z�G�1   GPS     Primary sampling: mixed [deeper than nominal 980dbar: discrete; nominal 980dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   B   @�  @�  @���A   A@  A`  A�  A�33A�  A�33A�33A�  A�  A�  B   B  B  B  B   B'��B/��B7��B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�33B�33B�  B�  B�  B�  B�  B�33B�33B�33B�33B�33B�  B���B�  B�  B���B���B���B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C�C�C
�C�C  C�fC  C  C  C�fC  C  C  C   C"�C$  C&  C(  C*  C,  C.  C0  C2  C4�C6  C8  C:�C<  C>  C@  CA�fCD  CE�fCG�fCJ�CL�CN  CP  CR  CT  CV�CX  CZ  C\  C]�fC`  Cb�Cd  Ce�fCh  Cj  Cl  Cn�Cp  Cq�fCt  Cv  Cx  Cz  C|�C~  C�fC�  C��C��C�  C�  C�  C��C��C�  C�  C��3C�  C��C�  C��3C�  C��C��C�  C��3C�  C��C�  C��3C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C��3C��3C�  C�  C�  C��3C��3C�  C�  C��C�  C�  C�  C��C�  C�  C�  C�  C�  C��C��C��C�  C��3C�  C�  C��C�  C��3C�  C�  C�  C�  C��3C�  C�  C��3C�  C��C��C�  C�  C�  C�  C�  C��3C��3C��3C��3C��3C�  C�  C��C�  C��3C�  C�  C�  C�  C��3C�  C��C�  C�  C�  C��C�  C�  C��3C��3C�  C��3C��3C�  C�  C��C�  C�  C��C��C��C��C��C��C��C�  C�  C�  C�  C�  C�  C�  D fD � D  D� DfD�fD  Dy�D  D� D  D� D  D� D��D� DfD�fD	  D	y�D
  D
�fDfD�fDfD� D��Dy�D��D� DfD�fDfD�fDfD�fDfD�fDfD�fD  Dy�D��Dy�D  D� DfD� D��D� D  D� D  D� D  D� D  D� D  D� DfD�fDfD� D��D � D!  D!�fD"fD"� D#  D#� D#��D$� D%  D%y�D&  D&� D'  D'�fD(  D(y�D)  D)� D*  D*� D+  D+� D+��D,y�D-  D-�fD.  D.� D/  D/� D0  D0y�D0��D1y�D1��D2y�D3  D3�fD4fD4� D5  D5� D6  D6� D7  D7y�D8  D8�fD9  D9� D:fD:�fD;  D;y�D<  D<� D=fD=� D=��D>� D?  D?� D@fD@� D@��DAy�DB  DB� DC  DC� DC��DD� DE  DE� DF  DF� DG  DGy�DH  DH� DI  DIy�DJ  DJ� DKfDK� DK��DL� DM  DM� DNfDN� DN��DO� DP  DP� DQ  DQ�fDRfDR� DR��DS� DT  DT� DUfDU�fDV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\�fD]fD]� D]��D^� D_  D_� D`  D`y�Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� De��Dfy�Dg  Dg�fDhfDh�fDi  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm�fDn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Ds�fDy��D�33D�\�D�� D��3D�33D�` D��3D��3D�  D�l�D���D��fD�fD�L�Dڳ3D�ٚD�	�D�Y�D�fD��f1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@�  @�  @���A   A@  A`  A�  A�33A�  A�33A�33A�  A�  A�  B   B  B  B  B   B'��B/��B7��B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�33B�33B�  B�  B�  B�  B�  B�33B�33B�33B�33B�33B�  B���B�  B�  B���B���B���B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C�C�C
�C�C  C�fC  C  C  C�fC  C  C  C   C"�C$  C&  C(  C*  C,  C.  C0  C2  C4�C6  C8  C:�C<  C>  C@  CA�fCD  CE�fCG�fCJ�CL�CN  CP  CR  CT  CV�CX  CZ  C\  C]�fC`  Cb�Cd  Ce�fCh  Cj  Cl  Cn�Cp  Cq�fCt  Cv  Cx  Cz  C|�C~  C�fC�  C��C��C�  C�  C�  C��C��C�  C�  C��3C�  C��C�  C��3C�  C��C��C�  C��3C�  C��C�  C��3C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C��3C��3C�  C�  C�  C��3C��3C�  C�  C��C�  C�  C�  C��C�  C�  C�  C�  C�  C��C��C��C�  C��3C�  C�  C��C�  C��3C�  C�  C�  C�  C��3C�  C�  C��3C�  C��C��C�  C�  C�  C�  C�  C��3C��3C��3C��3C��3C�  C�  C��C�  C��3C�  C�  C�  C�  C��3C�  C��C�  C�  C�  C��C�  C�  C��3C��3C�  C��3C��3C�  C�  C��C�  C�  C��C��C��C��C��C��C��C�  C�  C�  C�  C�  C�  C�  D fD � D  D� DfD�fD  Dy�D  D� D  D� D  D� D��D� DfD�fD	  D	y�D
  D
�fDfD�fDfD� D��Dy�D��D� DfD�fDfD�fDfD�fDfD�fDfD�fD  Dy�D��Dy�D  D� DfD� D��D� D  D� D  D� D  D� D  D� D  D� DfD�fDfD� D��D � D!  D!�fD"fD"� D#  D#� D#��D$� D%  D%y�D&  D&� D'  D'�fD(  D(y�D)  D)� D*  D*� D+  D+� D+��D,y�D-  D-�fD.  D.� D/  D/� D0  D0y�D0��D1y�D1��D2y�D3  D3�fD4fD4� D5  D5� D6  D6� D7  D7y�D8  D8�fD9  D9� D:fD:�fD;  D;y�D<  D<� D=fD=� D=��D>� D?  D?� D@fD@� D@��DAy�DB  DB� DC  DC� DC��DD� DE  DE� DF  DF� DG  DGy�DH  DH� DI  DIy�DJ  DJ� DKfDK� DK��DL� DM  DM� DNfDN� DN��DO� DP  DP� DQ  DQ�fDRfDR� DR��DS� DT  DT� DUfDU�fDV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\�fD]fD]� D]��D^� D_  D_� D`  D`y�Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� De��Dfy�Dg  Dg�fDhfDh�fDi  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm�fDn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Ds�fDy��D�33D�\�D�� D��3D�33D�` D��3D��3D�  D�l�D���D��fD�fD�L�Dڳ3D�ٚD�	�D�Y�D�fD��f1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��A���A���A���A���A���A���A���A���A���A���A��A��A��A��A�A���A��;A��#A��A��A��#A�ȴA���AƼjAƼjAƸRAƺ^AƸRAư!AƮAƩ�AƮAơ�AƗ�AƓuAƋDA�r�A�\)A�Q�A�(�A���AŋDA��RA��\A���A��`A���A��A��7A��-A�p�A�M�A�ƨA�7LA�bA�ȴA���A��`A��TA���A���A��A���A�t�A��DA�"�A�1A��TA��
A�jA�~�A���A�+A��HA�?}A��TA��DA�(�A�jA��uA�hsA�t�A�{A��A�=qA�z�A�ȴA��A�hsA�z�A�oA�A�+A��A�ƨA�ȴA�r�A�z�A���A��-A�jA��PA�$�A~�`A}��AzĜAw�Atv�AsAp�An��An5?Am��Am
=Ai�TAf$�Ab�A`ZA]��A[\)AY�AYXAX�!AW�-AV�uAR��AQ
=AP�DAO`BAM|�ALffAI�
AF�AE|�AC�AB�uAA33A@��A?�
A?�A>�A>5?A<��A:��A9G�A8=qA7�A6bA4��A4A�A41A3�hA0�jA.��A.�A-�;A-�A,�DA,{A+
=A*jA)�wA'S�A&�+A&v�A&1A%S�A$~�A#��A"��A"bA r�A�^AdZA^5A(�A�^Ar�A�A�A��A^5A�A�\AA`BA1A��A�7AdZA"�A�!An�A(�A��A\)A�A�A�A�/A�9A�A��AK�A
�9A
(�A	�TA	AbA"�AbAjAhsA�9A-AA|�A �9@���@�V@�Ĝ@��@��/@�ȴ@�-@��/@���@�E�@�h@��D@�l�@��@���@��@�u@�K�@�-@�V@�~�@�+@ܴ9@�S�@ف@��H@֏\@�J@��@�b@�{@�b@϶F@�o@ΰ!@�n�@́@�V@�;d@�v�@�/@��;@ÍP@§�@�=q@��-@�V@��/@�Ĝ@��@�9X@�b@��@��
@�t�@�v�@��@�(�@��m@���@���@�S�@�V@�hs@��9@�bN@��D@�O�@�`B@��-@��@�M�@�ff@�@��^@���@�@��#@���@�j@��m@�+@�^5@���@�M�@���@���@�?}@��@��F@��/@�1'@�  @���@�1@��;@��F@�t�@�@�^5@��@��-@�x�@��/@�A�@��@�1@��m@��w@�\)@���@��!@�n�@�^5@�ff@��\@�v�@��R@�"�@��F@�Ĝ@��-@�@�p�@�/@��9@�Q�@��;@��@�t�@�K�@�"�@���@��+@��@��#@���@�?}@���@�z�@�1@���@�l�@�"�@���@��y@���@���@��+@�n�@�V@�5?@��@��T@��@��u@��;@���@�\)@�;d@��@�@��H@���@�v�@�$�@��-@�G�@�1'@��P@�o@���@�^5@��@���@�O�@�/@�Ĝ@���@�bN@��@�t�@��@�9X@��
@��y@�ȴ@�~�@��-@�O�@��@��@��@�&�@�7L@�V@��D@��m@��@�|�@�S�@�
=@�V@��@�@��h@�7L@��9@�9X@��@��@���@�@��T@�@���@��h@�x�@���@�r�@�Q�@��@�1@�  @��
@�\)@�"�@���@���@���@�v�@�ff@�=q@��@���@��^@�O�@��@�z�@�  @��;@���@�l�@�dZ@�S�@��@��@���@���@��R@��+@���@�X@���@���@���@�r�@�z�@���@��9@�Ĝ@��@�V@���@�I�@�1'@�(�@�A�@��j@z�@s@h��@`1'@W
=@O�w@G��@B�!@:�@4j@0��@*�\@$(�@�;@��@�P@n�@��@	hs@�h1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��A���A���A���A���A���A���A���A���A���A���A��A��A��A��A�A���A��;A��#A��A��A��#A�ȴA���AƼjAƼjAƸRAƺ^AƸRAư!AƮAƩ�AƮAơ�AƗ�AƓuAƋDA�r�A�\)A�Q�A�(�A���AŋDA��RA��\A���A��`A���A��A��7A��-A�p�A�M�A�ƨA�7LA�bA�ȴA���A��`A��TA���A���A��A���A�t�A��DA�"�A�1A��TA��
A�jA�~�A���A�+A��HA�?}A��TA��DA�(�A�jA��uA�hsA�t�A�{A��A�=qA�z�A�ȴA��A�hsA�z�A�oA�A�+A��A�ƨA�ȴA�r�A�z�A���A��-A�jA��PA�$�A~�`A}��AzĜAw�Atv�AsAp�An��An5?Am��Am
=Ai�TAf$�Ab�A`ZA]��A[\)AY�AYXAX�!AW�-AV�uAR��AQ
=AP�DAO`BAM|�ALffAI�
AF�AE|�AC�AB�uAA33A@��A?�
A?�A>�A>5?A<��A:��A9G�A8=qA7�A6bA4��A4A�A41A3�hA0�jA.��A.�A-�;A-�A,�DA,{A+
=A*jA)�wA'S�A&�+A&v�A&1A%S�A$~�A#��A"��A"bA r�A�^AdZA^5A(�A�^Ar�A�A�A��A^5A�A�\AA`BA1A��A�7AdZA"�A�!An�A(�A��A\)A�A�A�A�/A�9A�A��AK�A
�9A
(�A	�TA	AbA"�AbAjAhsA�9A-AA|�A �9@���@�V@�Ĝ@��@��/@�ȴ@�-@��/@���@�E�@�h@��D@�l�@��@���@��@�u@�K�@�-@�V@�~�@�+@ܴ9@�S�@ف@��H@֏\@�J@��@�b@�{@�b@϶F@�o@ΰ!@�n�@́@�V@�;d@�v�@�/@��;@ÍP@§�@�=q@��-@�V@��/@�Ĝ@��@�9X@�b@��@��
@�t�@�v�@��@�(�@��m@���@���@�S�@�V@�hs@��9@�bN@��D@�O�@�`B@��-@��@�M�@�ff@�@��^@���@�@��#@���@�j@��m@�+@�^5@���@�M�@���@���@�?}@��@��F@��/@�1'@�  @���@�1@��;@��F@�t�@�@�^5@��@��-@�x�@��/@�A�@��@�1@��m@��w@�\)@���@��!@�n�@�^5@�ff@��\@�v�@��R@�"�@��F@�Ĝ@��-@�@�p�@�/@��9@�Q�@��;@��@�t�@�K�@�"�@���@��+@��@��#@���@�?}@���@�z�@�1@���@�l�@�"�@���@��y@���@���@��+@�n�@�V@�5?@��@��T@��@��u@��;@���@�\)@�;d@��@�@��H@���@�v�@�$�@��-@�G�@�1'@��P@�o@���@�^5@��@���@�O�@�/@�Ĝ@���@�bN@��@�t�@��@�9X@��
@��y@�ȴ@�~�@��-@�O�@��@��@��@�&�@�7L@�V@��D@��m@��@�|�@�S�@�
=@�V@��@�@��h@�7L@��9@�9X@��@��@���@�@��T@�@���@��h@�x�@���@�r�@�Q�@��@�1@�  @��
@�\)@�"�@���@���@���@�v�@�ff@�=q@��@���@��^@�O�@��@�z�@�  @��;@���@�l�@�dZ@�S�@��@��@���@���@��R@��+@���@�X@���@���@���@�r�@�z�@���@��9@�Ĝ@��@�V@���@�I�@�1'@�(�@�A�@��j@z�@s@h��@`1'@W
=@O�w@G��@B�!@:�@4j@0��@*�\@$(�@�;@��@�P@n�@��@	hs@�h1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�=B��B��B��B��B��B��B��B��B��B��B��B��B��B�oB�hB�PB�DB�+B�%B�B~�Bx�BiyB^5B>wB��B�TBB&�B�B��B�B�yB�mB�`B�)B��B�jB��B�hB�\B~�BhsBYBN�B8RB�BJB1B
��B
�yB
��B
�FB
�B
��B
x�B
O�B
<jB
+B
�B
	7B	��B	�B	�B	ÖB	��B	��B	�PB	~�B	x�B	r�B	iyB	XB	A�B	-B	�B	
=B	B��B��B�B�B�yB�#B��BɺB��B�XB�'B�B��B��B��B��B��B��B��B��B�{B�bB�PB�DB�=B�%B�B�B�B�B~�Bz�B� B� B� B� B� B}�B}�B�B~�B}�B�B�B� B~�B}�B}�Bz�By�B{�B�B�B�B� B� B� B}�B}�B{�By�Bv�Bs�Bo�Bk�BhsBffBe`BdZBcTBbNBaHB`BB^5B]/B\)BZBYBXBVBS�BQ�BQ�BO�BN�BM�BJ�BI�BF�BD�BA�B@�B@�B?}B?}B>wB=qB<jB9XB:^B;dB<jB>wBB�BA�BB�BC�BF�BF�BF�BG�BG�BG�BD�BB�B@�B>wB;dB9XB33B2-B0!B.B0!B.B-B-B,B.B0!B0!B0!B/B-B,B0!B6FB49B49B5?B5?B6FB6FB7LB9XB9XB9XB9XB9XB9XB9XB9XB;dB?}BD�BG�BH�BI�BN�BP�BN�BO�BS�BYB]/BdZBgmBk�Bp�Br�Bu�Bz�B�B�+B�JB�uB��B��B��B��B�{B�{B�bB�bB�\B�VB�JB�7B�1B�PB�bB�{B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�9B�jBƨB��B��B�/B�HB�ZB�B�B��B	B	PB	bB	�B	{B	�B	!�B	&�B	&�B	'�B	(�B	)�B	+B	-B	2-B	2-B	49B	6FB	7LB	8RB	;dB	>wB	@�B	@�B	@�B	@�B	A�B	B�B	C�B	D�B	E�B	E�B	E�B	F�B	J�B	M�B	R�B	R�B	S�B	S�B	T�B	T�B	T�B	S�B	S�B	S�B	S�B	S�B	VB	YB	[#B	[#B	\)B	^5B	aHB	cTB	dZB	e`B	jB	k�B	l�B	o�B	y�B	|�B	{�B	~�B	�B	�%B	�+B	�1B	�1B	�1B	�7B	�7B	�=B	�PB	�VB	�\B	�VB	�VB	�PB	�PB	�PB	�JB	�JB	�DB	�DB	�PB	�hB	�{B	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�!B	�'B	�-B	�3B	�?B	�RB	�jB	�wB	��B	��B	B	ĜB	ŢB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�B	�B	�B	�)B	�/B	�5B	�;B	�BB	�sB	��B
B
VB
�B
"�B
+B
2-B
8RB
?}B
F�B
I�B
O�B
W
B
[#B
bNB
e`B
jB
o�B
s�B
x�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�XG�O�B�B��B�B�B�B��B��B��B��B��B��B��B��B��B��B��B�oB�bB�=B�JB�Br�Bp�B\)B��B�BB
=B<jB&�BJB��B�B�B�B�yB�mB��B��B��B��B�VBt�BffBffBH�B$�B{B�B%B
��B
�NB
ƨB
��B
�3B
�{B
`BB
M�B
:^B
.B
�B
B
  B	�B	�B	�FB	�B	��B	�B	~�B	}�B	� B	r�B	W
B	?}B	)�B	�B	
=B��B��B��B��B	B�`B�B��B��BŢBĜB�wB�3B�!B�B��B��B��B��B��B��B��B��B��B�oB�VB�JB�7B�+B�%B�+B�PB�=B�B�B�%B�B�B�B�+B�+B�JB�+B�B�B�B�B�B�B� B�B�1B�+B�+B�B�B�1B�B�B~�B|�B{�B}�B{�Bu�Bo�BhsBffBffBe`Be`BcTBbNBaHBaHB`BBaHB_;BZBZB^5B\)BVBS�BR�BQ�BQ�BP�BM�BM�BK�BG�BE�BC�BB�BA�BC�BB�B?}BA�BB�BD�BD�BF�BF�BH�BF�BI�BJ�BK�BM�BO�BN�BJ�BG�BE�BD�BF�BD�B;dB8RB7LB5?B2-B1'B1'B2-B33B49B2-B2-B2-B2-B-B8RB9XB9XB49B49B7LB5?B8RB8RB9XB:^B:^B:^B9XB:^B9XB9XB<jB;dBC�BD�BG�BH�BI�BP�BP�BN�BO�BT�BYB]/BdZBgmBk�Bp�Br�Bu�Bz�B�B�+B�JB�uB��B��B��B��B��B�{B�oB�hB�hB�bB�oB�hB�=B�VB�bB�{B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�'B�FB�jBǮB��B��B�/B�HB�ZB�B�B��B	B	PB	hB	�B	{B	�B	!�B	'�B	'�B	(�B	)�B	+B	+B	-B	2-B	2-B	49B	7LB	7LB	8RB	<jB	?}B	A�B	A�B	@�B	A�B	A�B	C�B	C�B	D�B	F�B	E�B	E�B	I�B	J�B	O�B	S�B	R�B	T�B	S�B	T�B	VB	VB	S�B	T�B	S�B	S�B	W
B	XB	[#B	\)B	[#B	]/B	`BB	bNB	dZB	e`B	ffB	jB	m�B	l�B	m�B	y�B	}�B	}�B	~�B	�B	�%B	�1B	�7B	�1B	�1B	�7B	�7B	�DB	�\B	�bB	�bB	�VB	�VB	�VB	�PB	�VB	�PB	�PB	�DB	�DB	�VB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�!B	�-B	�3B	�9B	�FB	�XB	�jB	�wB	��B	��B	B	ĜB	ŢB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�
B	�B	�#B	�/B	�/B	�5B	�;B	�BB	�sB	��B
B
VB
�B
"�B
+B
2-B
8RB
?}B
F�B
J�B
O�B
W
B
[#B
bNB
e`B
jB
o�B
t�B
x�1111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<e`B=<jG�O�<49X<#�
<u<�t�<u<#�
<#�
<#�
<#�
<#�
<#�
<#�
<D��<#�
<D��<49X<49X<#�
<#�
<T��<#�
<#�
<�t�<�h<#�
<#�
<#�
<�1<�C�<e`B<#�
<#�
<#�
<#�
<T��<�j<ě�<49X<#�
<���<u<D��<T��<�j<�o<49X<#�
<e`B<T��<�o<�C�<�o<��
<�`B<�/<�o<�C�<u<���<��
<D��<�1<�1<�t�<D��<u<#�
<#�
<#�
<49X<�9X<���<�1<�t�<�t�<e`B<#�
<#�
<#�
<#�
<e`B<�j<#�
<#�
<49X<e`B<D��<���<�C�<49X<49X<#�
<#�
<#�
<#�
<#�
<#�
<#�
<49X<T��<49X<#�
<#�
<#�
<#�
<#�
<#�
<#�
<�t�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<e`B<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<D��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<49X<49X<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<D��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0 dbar.                                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201031447152012010314471520120103144715  AO  ARGQ                                                                        20111130141828  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130141828  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120103144715  IP                  G�O�G�O�G�O�                