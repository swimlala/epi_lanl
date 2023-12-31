CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2021-01-16T03:41:00Z creation;2021-01-16T03:41:02Z conversion to V3.1;2023-06-29T05:47:29Z update;     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         BPRIMARY|https://orcid.org/0000-0001-9150-6442|Kanako Sato, JAMSTEC        @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    7,   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    7<   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    7@   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    7D   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7T   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7d   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7t   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7|   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7�   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        8,   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    80   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    84   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     88   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8X   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8\   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8`   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8�   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8�   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8�   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    9    CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        :    PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  :   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ]$   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  a   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �,   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �4   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �T   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ހ   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �    HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �P   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �`   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �d   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �t   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �x   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �|   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20210116034100  20230705041505  5905058                                                                 JAMSTEC                                                         PRES            TEMP            PSAL              !A   JA  I2_0675_289                     2C  D   NAVIS_A                         0675                            ARGO 102115                     863 @�V�pB� 1   @�V�UUU�@6m�M:��b�n��1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�33B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C��C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5y�D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM�fDN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D���D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�C3DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D��3D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�3D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�(�@���Az�A(z�AHz�Ahz�A�=qA�=qA�=qA�=qA�=qA�=qA�=qA�=qB�B
�B�B�B"�B*�B2�B:�BB�BJ�BR�BZ�Bb�Bj�Br�Bz�B�\B�\B�\B�B�B�\B�\B�\B�\B�\B��)B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\C ��C��C��C��C��C
��C��C��C��C��C��C��C��C��C��C��C ��C"��C$��C&��C(��C*��C,��C.��C0��C2��C4��C6��C8��C:��C<��C>��C@��CB��CD��CF��CH��CJ��CL��CN��CP��CR��CT��CV��CX��CZ��C\��C^��C`��Cb��Cd��Cf��Ch��Cj��Cl��Cn��Cp��Cr��Ct��Cv��Cx��Cz��C|��C~��C�C�C�C�C�C�C�C�C�P�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�7
C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�P�C�C�C�C�C�P�C�C�C�C�C�C�C�C�C�C�D !�D ��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D	!�D	��D
!�D
��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D !�D ��D!!�D!��D"!�D"��D#!�D#��D$!�D$��D%!�D%��D&!�D&��D'!�D'��D(!�D(��D)!�D)��D*!�D*��D+!�D+��D,!�D,��D-!�D-��D.!�D.��D/!�D/��D0!�D0��D1!�D1��D2!�D2��D3!�D3��D4!�D4��D5!�D5��D6!�D6��D7!�D7��D8!�D8��D9!�D9��D:!�D:��D;!�D;��D<!�D<��D=!�D=��D>!�D>��D?!�D?��D@!�D@��DA!�DA��DB!�DB��DC!�DC��DD!�DD��DE!�DE��DF!�DF��DG!�DG��DH!�DH��DI!�DI��DJ!�DJ��DK!�DK��DL!�DL��DM!�DM�RDN!�DN��DO!�DO��DP!�DP��DQ!�DQ��DR!�DR��DS!�DS��DT!�DT��DU!�DU��DV!�DV��DW!�DW��DX!�DX��DY!�DY��DZ!�DZ��D[!�D[��D\!�D\��D]!�D]��D^!�D^��D_!�D_��D`!�D`��Da!�Da��Db!�Db��Dc!�Dc��Dd!�Dd��De!�De��Df!�Df��Dg!�Dg��Dh!�Dh��Di!�Di��Dj!�Dj��Dk!�Dk��Dl!�Dl��Dm!�Dm��Dn!�Dn��Do!�Do��Dp!�Dp��Dq!�Dq��Dr!�Dr��Ds!�Ds��Dt!�Dt��Du!�Du��Dv!�Dv��Dw!�Dw��Dx!�Dx��Dy!�Dy��Dz!�Dz��D{!�D{��D|!�D|��D}!�D}��D~!�D~��D!�D��D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D�D���D��D�P�DÐ�D���D��D�P�DĐ�D���D��D�P�DŐ�D���D��D�P�DƐ�D���D��D�P�Dǐ�D���D��D�P�DȐ�D���D��D�P�Dɐ�D���D��D�P�Dʐ�D���D��D�P�Dː�D���D��D�P�D̐�D���D��D�P�D͐�D���D��D�P�Dΐ�D���D��D�P�Dϐ�D���D��D�P�DА�D���D��D�P�Dѐ�D���D��D�P�DҐ�D���D��D�P�DӐ�D���D��D�T)DԐ�D���D��D�P�DՐ�D���D��D�P�D֐�D���D��D�P�Dא�D��)D��D�P�Dؐ�D���D��D�P�Dِ�D���D��D�P�Dڐ�D���D�)D�P�Dې�D���D��D�P�Dܐ�D���D��D�P�Dݐ�D���D��D�P�Dސ�D���D��D�P�Dߐ�D���D��D�P�D���D���D��D�P�D��D���D��D�P�D��D���D��D�P�D��D���D��D�P�D��D���D��D�P�D��D���D��D�P�D��D���D��D�P�D��D���D��D�P�D��D���D��D�P�D��D���D��D�P�D��D���D��D�P�D��D���D��D�P�D��D���D��D�P�D��D���D��D�P�D��D���D��D�P�D��D���D��D�P�D��D���D��D�P�D��D���D��D�P�D��D���D��D�P�D��D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D��)D� �111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�ĜA�A�AŰ!Aŕ�AōPAōPAŋDAŋDAŉ7AŇ+Aŉ7Aŉ7AŇ+Aŉ7AŃAŃAŅAŅAŅAŃAŃAŅAŁA�~�A�~�AŃA�~�AŁA�~�A�z�A�z�A�|�A�|�A�|�A�t�A�ZA��A�`BA×�A�O�A��A��A��A�9XA�-A��\A��TA��A�x�A�-A���A�I�A���A�bA��9A���A�K�A�%A��A�VA���A�"�A�$�A�
=A��hA��!A���A�7LA��PA�%A�XA���A�ZA��mA���A�&�A�|�A��hA���A�A�$�A��yA��`A���A�I�A��mA�$�A�x�A�
=A��;A��TA���A��A���A��A���A�n�A�A��A� �A�A}|�A|�A|bA{33AyAv��At9XArVAp�\AooAm\)Ai�-AdbAa��A`bNA\1'AZ�AY�AV��AR�+AN��AK7LAI�-AG`BAE�7ADM�ACAB�\A?�-A=�hA=+A<�A<=qA;��A;A:��A9��A8$�A6ZA5VA3�7A2�!A1O�A/x�A/
=A.�A-
=A+��A+VA)�#A(ĜA'�A&��A%�PA$n�A#S�A"E�A!��A ȴA =qA��Ar�A?}A��AQ�A��AI�AO�A33A��A��AbNA�AM�AG�A�^AoAM�A�`AC�A�RA9XA��A|�A��A�A
��A
1'A	VA~�A$�A�^A�A�PAI�AAVA~�A��A bN@��\@���@�V@�I�@���@��@���@�%@�(�@�|�@��y@��@�V@��D@�C�@��@�@�\)@�^@��`@�ƨ@�V@��`@��;@�~�@�r�@ߝ�@��@�&�@���@�A�@۝�@�=q@���@�%@�Q�@�1@��m@�l�@֧�@�J@�X@ԋD@�S�@��@��@��`@̃@��@�O�@�t�@�V@��#@�`B@�%@Ĭ@�A�@å�@�@�^5@�=q@�{@�O�@���@�l�@��H@��+@��`@��@�(�@�\)@�V@��@�V@�r�@�\)@�^5@���@���@���@� �@��@��R@�x�@�O�@���@�v�@�=q@�{@�O�@�Ĝ@��@��
@��P@�;d@��@��R@��@��@�bN@���@���@�^5@�=q@��@��#@���@��#@���@���@��T@���@���@�z�@�b@��m@���@�;d@��@���@�$�@��@��^@�%@���@��j@��@��D@�j@�I�@��@�|�@�C�@�o@���@��\@�ff@�=q@�J@��#@��-@���@��@�hs@��@��@��/@�Ĝ@��@�I�@�1@�  @���@��m@�ƨ@�l�@�K�@�"�@�^5@�E�@�{@��^@���@���@��7@�/@�%@���@��`@�bN@��@�ƨ@���@���@��@�t�@�dZ@�33@�
=@��@���@��@���@��@��T@�@��@�%@���@�Q�@��
@���@��P@��P@�t�@�|�@�l�@�@��!@�M�@���@���@�p�@�O�@�%@��/@�Ĝ@��D@�Q�@���@��@�\)@�;d@��@��y@��R@���@��+@�$�@���@�/@�%@��@��j@���@�r�@�(�@� �@�b@��m@�ƨ@��F@��@�S�@��@��R@���@���@�n�@�$�@���@��T@���@�hs@�X@�O�@�G�@�&�@��@���@���@��j@���@��u@��@�r�@�Z@�A�@�(�@� �@��@�b@�1@�  @��m@��;@�|�@�+@���@���@�n�@�V@�5?@�$�@���@�`B@�j@� �@�1@��F@���@��P@�|�@�t�@�S�@�33@���@��H@���@�ȴ@���@��!@�n�@��@���@�hs@�&�@�V@�%@���@��/@���@�r�@�j@�Z@�b@�@�P@~�y@~5?@}��@}�@|��@|�D@|j@|I�@{��@{C�@{"�@z~�@y�^@yhs@y&�@x�9@xQ�@x  @w�w@w|�@w
=@v��@vff@u��@t�/@tj@s�m@s��@sdZ@so@r�@r�\@r-@q�@q�7@qG�@pĜ@pQ�@o�;@o��@o|�@o;d@o�@n�@nE�@m@mp�@m�@m�@mV@l�D@l�@k��@kdZ@k@j��@j^5@j=q@jJ@i��@ihs@i&�@i&�@i%@h��@hr�@hb@g�@g�w@g�P@g\)@fȴ@f��@f5?@e�-@e@e�@e�@d�j@dj@cS�@c@b�!@b�@a��@aX@`�u@_K�@^�@^�R@^ff@^$�@]�@]@]/@\z�@[�F@[�@[�@[C�@Z��@Z��@ZJ@Y�7@Yx�@Yhs@Y&�@X��@XQ�@X  @W�w@Vȴ@V�+@V$�@U��@U�-@U�h@Up�@UO�@T��@Tj@T9X@T�@S��@S�
@S��@S�@SS�@S"�@R�\@R-@RJ@Q��@Q��@QG�@P�9@P �@O��@O�@N�y@N�R@N��@Nff@N{@M�@M@M?}@L��@L�/@Lz�@LZ@K�
@KC�@K@J��@J��@Jn�@I�#@Ix�@H�`@H��@H��@H�u@Hr�@HA�@H  @G��@G�@Fv�@FE�@F5?@F$�@F{@E��@E�-@E�h@E?}@Dj@C�
@Ct�@C"�@B��@Bn�@B-@A�@A�7@@��@@�u@@A�@?�;@?K�@>�y@>�R@>��@>V@=�@=�T@=@=�h@=?}@=�@<��@<�@<Z@<9X@;�m@;�F@;�@;dZ@;S�@;33@;"�@;@:�@:�@:�H@:��@:��@:�!@:��@:^5@9��@9��@9x�@9hs@97L@8��@8��@8bN@8A�@8 �@7��@7l�@6�@6�+@6v�@6V@6@5�-@5��@5�h@5`B@5�@4�@4�j@4�D@4I�@4�@3��@3ƨ@3S�@2�@2n�@2=q@2J@1�@1�^@1�7@1G�@17L@1�@1%@1%@0��@0�u@0bN@0Q�@01'@/��@/��@/l�@/�@.��@.�y@.�+@.@-��@-/@,��@,�j@,�@,�D@,j@,�@+��@+�
@+�@+t�@+dZ@+dZ@+dZ@+33@+o@+@+@*�@*�!@*~�@*J@)�@)�#@)��@)��@)�7@)x�@)G�@)%@(Ĝ@(1'@(  @'�;@'�@'�P@'\)@'\)@'K�@'
=@&�y@&��@&V@&{@%��@%�h@%/@$��@$��@$�@$�j@$�@$�@$��@$z�@$(�@$�@$1@#��@#�m@#�
@#��@#dZ@#C�@#C�@#33@"��@"-@"J@!�@!��@!hs@!�@ �`@ ��@ Ĝ@ ��@ �u@ �@ �@ A�@   @�@�@\)@��@��@�y@ȴ@��@�+@v�@V@@�-@�h@`B@O�@��@�D@z�@z�@I�@1@1@1@��@�m@�
@ƨ@�F@��@S�@"�@�H@�\@�\@~�@n�@^5@=q@�@x�@X@��@Q�@ �@�;@��@|�@+@ff@$�@@@�-@�h@p�@/@��@�j@z�@I�@I�@9X@1@��@�
@��@33@o@@�@�H@��@~�@-@�@��@G�@�`@Ĝ@A�@�@�w@��@�P@l�@�@�y@��@ff@E�@{@@@�@�-@`B@�@�/@��@�@�@�j@�D@I�@�@ƨ@�@t�@S�@33@"�@
�@
�H@
��@
~�@
^5@
-@	��@	hs@	%@��@��@�@bN@1'@�@�@�@�R111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�ĜA�A�AŰ!Aŕ�AōPAōPAŋDAŋDAŉ7AŇ+Aŉ7Aŉ7AŇ+Aŉ7AŃAŃAŅAŅAŅAŃAŃAŅAŁA�~�A�~�AŃA�~�AŁA�~�A�z�A�z�A�|�A�|�A�|�A�t�A�ZA��A�`BA×�A�O�A��A��A��A�9XA�-A��\A��TA��A�x�A�-A���A�I�A���A�bA��9A���A�K�A�%A��A�VA���A�"�A�$�A�
=A��hA��!A���A�7LA��PA�%A�XA���A�ZA��mA���A�&�A�|�A��hA���A�A�$�A��yA��`A���A�I�A��mA�$�A�x�A�
=A��;A��TA���A��A���A��A���A�n�A�A��A� �A�A}|�A|�A|bA{33AyAv��At9XArVAp�\AooAm\)Ai�-AdbAa��A`bNA\1'AZ�AY�AV��AR�+AN��AK7LAI�-AG`BAE�7ADM�ACAB�\A?�-A=�hA=+A<�A<=qA;��A;A:��A9��A8$�A6ZA5VA3�7A2�!A1O�A/x�A/
=A.�A-
=A+��A+VA)�#A(ĜA'�A&��A%�PA$n�A#S�A"E�A!��A ȴA =qA��Ar�A?}A��AQ�A��AI�AO�A33A��A��AbNA�AM�AG�A�^AoAM�A�`AC�A�RA9XA��A|�A��A�A
��A
1'A	VA~�A$�A�^A�A�PAI�AAVA~�A��A bN@��\@���@�V@�I�@���@��@���@�%@�(�@�|�@��y@��@�V@��D@�C�@��@�@�\)@�^@��`@�ƨ@�V@��`@��;@�~�@�r�@ߝ�@��@�&�@���@�A�@۝�@�=q@���@�%@�Q�@�1@��m@�l�@֧�@�J@�X@ԋD@�S�@��@��@��`@̃@��@�O�@�t�@�V@��#@�`B@�%@Ĭ@�A�@å�@�@�^5@�=q@�{@�O�@���@�l�@��H@��+@��`@��@�(�@�\)@�V@��@�V@�r�@�\)@�^5@���@���@���@� �@��@��R@�x�@�O�@���@�v�@�=q@�{@�O�@�Ĝ@��@��
@��P@�;d@��@��R@��@��@�bN@���@���@�^5@�=q@��@��#@���@��#@���@���@��T@���@���@�z�@�b@��m@���@�;d@��@���@�$�@��@��^@�%@���@��j@��@��D@�j@�I�@��@�|�@�C�@�o@���@��\@�ff@�=q@�J@��#@��-@���@��@�hs@��@��@��/@�Ĝ@��@�I�@�1@�  @���@��m@�ƨ@�l�@�K�@�"�@�^5@�E�@�{@��^@���@���@��7@�/@�%@���@��`@�bN@��@�ƨ@���@���@��@�t�@�dZ@�33@�
=@��@���@��@���@��@��T@�@��@�%@���@�Q�@��
@���@��P@��P@�t�@�|�@�l�@�@��!@�M�@���@���@�p�@�O�@�%@��/@�Ĝ@��D@�Q�@���@��@�\)@�;d@��@��y@��R@���@��+@�$�@���@�/@�%@��@��j@���@�r�@�(�@� �@�b@��m@�ƨ@��F@��@�S�@��@��R@���@���@�n�@�$�@���@��T@���@�hs@�X@�O�@�G�@�&�@��@���@���@��j@���@��u@��@�r�@�Z@�A�@�(�@� �@��@�b@�1@�  @��m@��;@�|�@�+@���@���@�n�@�V@�5?@�$�@���@�`B@�j@� �@�1@��F@���@��P@�|�@�t�@�S�@�33@���@��H@���@�ȴ@���@��!@�n�@��@���@�hs@�&�@�V@�%@���@��/@���@�r�@�j@�Z@�b@�@�P@~�y@~5?@}��@}�@|��@|�D@|j@|I�@{��@{C�@{"�@z~�@y�^@yhs@y&�@x�9@xQ�@x  @w�w@w|�@w
=@v��@vff@u��@t�/@tj@s�m@s��@sdZ@so@r�@r�\@r-@q�@q�7@qG�@pĜ@pQ�@o�;@o��@o|�@o;d@o�@n�@nE�@m@mp�@m�@m�@mV@l�D@l�@k��@kdZ@k@j��@j^5@j=q@jJ@i��@ihs@i&�@i&�@i%@h��@hr�@hb@g�@g�w@g�P@g\)@fȴ@f��@f5?@e�-@e@e�@e�@d�j@dj@cS�@c@b�!@b�@a��@aX@`�u@_K�@^�@^�R@^ff@^$�@]�@]@]/@\z�@[�F@[�@[�@[C�@Z��@Z��@ZJ@Y�7@Yx�@Yhs@Y&�@X��@XQ�@X  @W�w@Vȴ@V�+@V$�@U��@U�-@U�h@Up�@UO�@T��@Tj@T9X@T�@S��@S�
@S��@S�@SS�@S"�@R�\@R-@RJ@Q��@Q��@QG�@P�9@P �@O��@O�@N�y@N�R@N��@Nff@N{@M�@M@M?}@L��@L�/@Lz�@LZ@K�
@KC�@K@J��@J��@Jn�@I�#@Ix�@H�`@H��@H��@H�u@Hr�@HA�@H  @G��@G�@Fv�@FE�@F5?@F$�@F{@E��@E�-@E�h@E?}@Dj@C�
@Ct�@C"�@B��@Bn�@B-@A�@A�7@@��@@�u@@A�@?�;@?K�@>�y@>�R@>��@>V@=�@=�T@=@=�h@=?}@=�@<��@<�@<Z@<9X@;�m@;�F@;�@;dZ@;S�@;33@;"�@;@:�@:�@:�H@:��@:��@:�!@:��@:^5@9��@9��@9x�@9hs@97L@8��@8��@8bN@8A�@8 �@7��@7l�@6�@6�+@6v�@6V@6@5�-@5��@5�h@5`B@5�@4�@4�j@4�D@4I�@4�@3��@3ƨ@3S�@2�@2n�@2=q@2J@1�@1�^@1�7@1G�@17L@1�@1%@1%@0��@0�u@0bN@0Q�@01'@/��@/��@/l�@/�@.��@.�y@.�+@.@-��@-/@,��@,�j@,�@,�D@,j@,�@+��@+�
@+�@+t�@+dZ@+dZ@+dZ@+33@+o@+@+@*�@*�!@*~�@*J@)�@)�#@)��@)��@)�7@)x�@)G�@)%@(Ĝ@(1'@(  @'�;@'�@'�P@'\)@'\)@'K�@'
=@&�y@&��@&V@&{@%��@%�h@%/@$��@$��@$�@$�j@$�@$�@$��@$z�@$(�@$�@$1@#��@#�m@#�
@#��@#dZ@#C�@#C�@#33@"��@"-@"J@!�@!��@!hs@!�@ �`@ ��@ Ĝ@ ��@ �u@ �@ �@ A�@   @�@�@\)@��@��@�y@ȴ@��@�+@v�@V@@�-@�h@`B@O�@��@�D@z�@z�@I�@1@1@1@��@�m@�
@ƨ@�F@��@S�@"�@�H@�\@�\@~�@n�@^5@=q@�@x�@X@��@Q�@ �@�;@��@|�@+@ff@$�@@@�-@�h@p�@/@��@�j@z�@I�@I�@9X@1@��@�
@��@33@o@@�@�H@��@~�@-@�@��@G�@�`@Ĝ@A�@�@�w@��@�P@l�@�@�y@��@ff@E�@{@@@�@�-@`B@�@�/@��@�@�@�j@�D@I�@�@ƨ@�@t�@S�@33@"�@
�@
�H@
��@
~�@
^5@
-@	��@	hs@	%@��@��@�@bN@1'@�@�@�@�R111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�XBB��B�B��B�B#�B$�B)�B+B-B.B/B0!B1'B6FB?}B9XB49B,B$�B�B�BPBVB{B �B�B�B"�B�B�BhBB�B�ZB��B�qB�B��B~�Bp�BbNBN�B@�B1'B�B�B{BPBB
��B
�B
�B
�HB
��B
ɺB
B
�-B
��B
�JB
v�B
e`B
T�B
C�B
/B
%�B
 �B
�B
VB	��B	�yB	�B	��B	��B	�'B	��B	r�B	bNB	W
B	>wB	.B	%�B	�B	  B�B�B��BĜB�qB�LB�3B�B��B��B��B��B��B��B�oB�bB�VB�=B�%B� B|�Bw�Bv�Bo�Bm�Bm�BiyBgmBdZBcTB`BB^5B\)BYBT�BS�BQ�BP�BP�BO�BN�BM�BL�BL�BK�BL�BN�BL�BK�BK�BJ�BJ�BG�BF�BE�B;dB6FB49B2-B49B2-B33B6FB5?B6FB6FB6FB6FB7LB5?B5?B33B49B2-B0!B.B.B.B.B.B,B+B+B+B,B-B/B/B1'B2-B33B5?B8RB:^B<jB<jB=qB=qB?}B?}BA�BC�BE�BG�BK�BJ�BO�BP�BN�BM�BL�BK�BM�BM�BO�BP�BP�BP�BQ�BVBXBYBYB\)BZB^5B_;B_;B_;BhsBjBm�Bm�Bn�Bo�Bp�Bq�Br�Bt�Bt�Bu�Bu�Bw�Bz�B}�B� B� B�B�%B�+B�7B�JB�PB�\B�\B�uB��B��B��B��B��B��B��B��B�B�B�B�B�B�-B�9B�?B�RB�XB�^B�dB�qBǮB��B��B��B�B�#B�/B�HB�NB�ZB�`B�fB�fB�sB�B�B�B��B��B��B	B	B	B	JB	VB	bB	�B	�B	�B	�B	�B	�B	�B	�B	#�B	$�B	&�B	)�B	+B	,B	.B	0!B	1'B	33B	49B	5?B	7LB	;dB	=qB	?}B	A�B	D�B	E�B	I�B	I�B	J�B	K�B	L�B	O�B	P�B	R�B	W
B	XB	\)B	_;B	`BB	`BB	bNB	ffB	gmB	gmB	gmB	hsB	jB	m�B	n�B	p�B	s�B	s�B	t�B	w�B	y�B	{�B	}�B	�B	�B	�B	�B	�%B	�7B	�DB	�PB	�bB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�-B	�-B	�3B	�3B	�9B	�FB	�FB	�LB	�XB	�qB	��B	��B	��B	B	ÖB	ĜB	ƨB	ƨB	ƨB	ǮB	ȴB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�
B	�B	�B	�B	�B	�B	�B	�B	�#B	�#B	�)B	�)B	�)B	�)B	�)B	�)B	�/B	�/B	�;B	�BB	�HB	�NB	�TB	�TB	�TB	�TB	�fB	�fB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
%B
+B
1B
	7B
	7B
	7B

=B

=B

=B
DB
DB
JB
JB
PB
PB
VB
VB
VB
VB
VB
\B
\B
bB
hB
hB
hB
hB
hB
oB
uB
uB
uB
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
 �B
 �B
 �B
 �B
!�B
!�B
!�B
!�B
"�B
#�B
$�B
$�B
$�B
$�B
$�B
$�B
%�B
&�B
&�B
&�B
&�B
'�B
'�B
(�B
(�B
)�B
)�B
+B
+B
+B
+B
+B
+B
,B
-B
-B
-B
-B
-B
-B
-B
.B
.B
/B
/B
/B
/B
0!B
0!B
1'B
2-B
2-B
33B
33B
33B
33B
33B
33B
33B
49B
5?B
5?B
5?B
5?B
5?B
6FB
6FB
7LB
7LB
7LB
7LB
8RB
8RB
9XB
9XB
9XB
9XB
9XB
9XB
:^B
:^B
;dB
<jB
<jB
<jB
<jB
<jB
=qB
=qB
=qB
=qB
?}B
?}B
?}B
?}B
@�B
@�B
A�B
A�B
A�B
B�B
B�B
B�B
C�B
D�B
D�B
D�B
D�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
F�B
F�B
F�B
F�B
F�B
F�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
H�B
H�B
H�B
H�B
H�B
I�B
I�B
I�B
J�B
J�B
J�B
J�B
K�B
K�B
K�B
K�B
L�B
L�B
L�B
L�B
L�B
L�B
L�B
L�B
M�B
M�B
M�B
M�B
M�B
N�B
N�B
O�B
O�B
O�B
P�B
P�B
P�B
P�B
P�B
P�B
P�B
P�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
R�B
R�B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
T�B
T�B
T�B
T�B
T�B
T�B
VB
VB
VB
VB
W
B
W
B
W
B
W
B
W
B
W
B
W
B
W
B
W
B
XB
XB
XB
XB
YB
YB
YB
YB
YB
YB
ZB
ZB
ZB
ZB
[#B
[#B
[#B
[#B
[#B
[#B
[#B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
]/B
]/B
]/B
]/B
]/B
]/B
]/B
^5B
^5B
^5B
^5B
^5B
^5B
^5B
^5B
_;B
_;B
_;B
`BB
`BB
`BB
`BB
`BB
`BB
`BB
`BB
`BB
`BB
`BB
`BB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
bNB
bNB
bNB
bNB
bNB
cTB
cTB
cTB
cTB
dZB
dZB
dZB
e`B
e`B
e`B
e`B
e`B
e`B
e`B
ffB
ffB
ffB
ffB
ffB
ffB
ffB
ffB
ffB
ffB
gmB
ffB
ffB
ffB
ffB
ffB
ffB
gmB
gmB
gmB
gmB
gmB
hsB
hsB
hsB
hsB
hsB
iyB
iyB
iyB
iyB
iyB
iyB
iyB
iyB
jB
jB
jB
jB
jB
jB
k�B
k�B
k�B
k�B
l�B
l�B
l�B
l�B
m�B
m�B
m�B
m�B
m�B
m�B
n�B
n�B
o�B
n�B
n�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
p�B
p�B
p�B
p�B
q�B
r�B
q�B
q�B
q�B
r�B
r�B
r�B
r�B
r�B
s�B
s�B
s�B
s�B
t�B
t�B
t�B
t�B
t�B
t�B
s�B
s�B
t�B
t�B
t�B
t�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�~B��B��B��B�-B��B�JBżBуB�B�B 'B'�B(sB+QB,qB.�B/ B/�B1�B4B9$B@�B:^B5�B-�B'mB!�BB"B�B�B"�B�B�B$ZB!B�B�BMB�B�8B�B�B��B��B�;BsBezBQ BB�B2�BVBQBgB�BSB
��B
�B
�B
�:B
��B
�DB
�B
�?B
��B
�BB
y>B
g�B
W�B
EmB
0B
&�B
!�B
�B
�B
 �B	�B	�#B	��B	�GB	��B	�-B	uZB	d�B	[#B	@�B	0UB	)_B	IB	3B�IB�)B�vBƨB��B�RB�B��B��B�B��B�1B�$B�9B�B��B�.B�JB��B��B~(By�Bx�BpUBn�Bn�Bj�BhsBe�Bd�BaHB_�B]dBZkBVSBUBR�BQ�BQ�BP�BPHBOBMjBM6BL�BNVBO�BL�BK�BL0BK^BLBH�BG�BGEB<PB7�B5�B3�B4�B2�B3�B6�B6FB7�B72B72B7fB7�B5�B5�B4�B5�B3�B0�B.�B.�B/B/�B/5B,qB+�B+�B,B-wB-�B/�B/�B1vB2aB3�B5�B8�B;0B="B="B>BB>]B?�B@BB[BDMBF?BH�BL�BKDBP}BQNBN�BN"BMBL~BNBN<BP.BQ BP�BQBR:BVSBXyBY�BZB]�B[�B^�B_pB_�B`�BiDBkBm�Bm�Bn�Bo�Bp�Bq�Br�Bt�Bt�Bu�Bv+Bx8B{dB~(B�OB��B�9B�?B��B��B�~B��B��B��B��B��B��B��B�B�:B�B��B�0B��B�wB��B�/B�}B�GB�9B�tB�RB�rB�^B��B�BBǮB��B�.B�MB��B�	B�/B�B�B�&B�,B�2B�2B�sB��B�B�B��B��B��B	 �B	-B	9B	0B	pB	�B	�B	B	kB	�B	�B	�B	�B	�B	#�B	$�B	&�B	)�B	*�B	,B	-�B	0B	1B	3B	4B	5%B	7LB	;0B	=VB	?HB	AoB	D�B	E�B	I�B	I�B	J�B	K�B	L�B	O�B	Q B	S&B	V�B	XB	\B	_B	`B	`'B	bhB	f2B	g8B	gRB	g�B	hsB	jeB	mwB	n}B	poB	s�B	s�B	t�B	w�B	y�B	{�B	}�B	��B	��B	��B	�B	�?B	�7B	�DB	�jB	�}B	�@B	�gB	�gB	�SB	�_B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	�B	��B	�B	�B	�B	�+B	�fB	�rB	��B	�OB	�OB	��B	�uB	�{B	āB	�tB	�tB	�tB	ǔB	ȀB	ȚB	ɠB	��B	��B	͹B	ΥB	οB	��B	бB	ѷB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	�B	��B	��B	�B	�B	��B	�IB	�!B	�BB	�HB	�4B	� B	� B	�:B	�nB	�B	�B	�eB	�kB	�B	�WB	�wB	�]B	�]B	�]B	�]B	�B	�iB	�iB	�B	�oB	��B	�B	��B	�B	��B	��B	�B	�tB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
 B
�B
�B
�B
�B
�B
�B
�B
B
�B
B
B
%B
B
B
	B
	B
	B

	B

=B

#B
B
)B
B
JB
6B
PB
<B
"B
<B
"B
VB
BB
BB
HB
NB
B
4B
NB
NB
TB
@B
[B
@B
aB
FB
FB
MB
MB
mB
SB
SB
SB
sB
YB
_B
_B
eB
eB
�B
kB
�B
�B
]B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
 �B
 �B
 �B
 �B
!�B
!�B
!�B
!�B
"�B
#�B
$�B
$�B
$�B
$�B
$�B
$�B
%�B
&�B
&�B
&�B
&�B
'�B
'�B
(�B
(�B
)�B
)�B
*�B
*�B
*�B
*�B
*�B
*�B
+�B
,�B
,�B
,�B
,�B
,�B
,�B
,�B
-�B
-�B
.�B
/ B
.�B
.�B
0B
0B
1B
2-B
2B
2�B
2�B
2�B
2�B
2�B
3B
2�B
49B
5B
5B
5B
5%B
5?B
6+B
6B
7B
7B
7B
72B
8RB
88B
9$B
9	B
9>B
9$B
9$B
9$B
:DB
:xB
;JB
<PB
<6B
<B
<PB
<6B
=<B
=VB
=qB
=qB
?cB
?cB
?HB
?HB
@OB
@OB
AUB
A�B
AoB
B[B
BuB
BuB
C{B
DgB
D�B
D�B
DgB
EmB
E�B
EmB
EmB
EmB
E�B
EmB
EmB
FtB
F�B
FtB
F�B
F�B
FtB
G_B
G�B
G_B
GzB
GzB
G_B
GzB
G_B
GzB
GzB
G�B
GzB
G�B
H�B
H�B
H�B
H�B
H�B
I�B
I�B
I�B
J�B
J�B
J�B
J�B
K�B
K�B
K�B
K�B
L�B
L�B
L�B
L�B
L�B
L�B
L�B
L�B
M�B
M�B
M�B
M�B
M�B
N�B
N�B
O�B
O�B
O�B
P�B
P�B
P�B
P�B
P�B
P�B
P�B
P�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
R�B
R�B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
T�B
T�B
T�B
T�B
T�B
T�B
U�B
U�B
U�B
U�B
V�B
V�B
V�B
V�B
V�B
V�B
V�B
V�B
V�B
W�B
W�B
W�B
W�B
X�B
YB
X�B
X�B
X�B
X�B
Y�B
Y�B
Y�B
ZB
Z�B
[	B
Z�B
[	B
[	B
Z�B
Z�B
[�B
[�B
[�B
[�B
[�B
[�B
[�B
[�B
\�B
\�B
\�B
\�B
\�B
\�B
\�B
^B
^B
]�B
^B
^5B
^B
^B
^B
_!B
_B
_B
`B
_�B
`B
`'B
`B
_�B
_�B
`'B
`'B
`B
`B
`'B
aB
`�B
`�B
aB
aB
`�B
a-B
aB
aB
aB
bB
b4B
b4B
b4B
b4B
cB
cB
c:B
c B
dB
dB
dB
e,B
eB
e,B
eB
e,B
eFB
e,B
fLB
fLB
fB
fB
f2B
f2B
fLB
f2B
ffB
fLB
gmB
ffB
f2B
f2B
f2B
f2B
fLB
gmB
g8B
gRB
gB
g8B
h>B
hXB
h>B
hXB
h>B
iDB
iDB
i*B
iDB
i_B
iDB
i_B
i_B
jeB
jeB
j0B
jKB
jKB
jKB
kQB
kkB
kQB
kQB
lWB
lWB
lWB
lqB
m]B
m]B
m]B
m]B
m]B
mwB
ncB
ncB
o�B
n}B
ncB
oOB
oiB
o�B
oiB
oiB
o�B
oiB
oiB
oiB
pUB
poB
poB
poB
q�B
r�B
qvB
q[B
q�B
r|B
r|B
r|B
r|B
r�B
s�B
s�B
s�B
s�B
t�B
t�B
t�B
t�B
t�B
t�B
s�B
s�B
t�B
t�B
t�B
t�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=-0.53(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         202101210034032021012100340320210121003403202306231727102023062317271020230623172710202101220026582021012200265820210122002658  JA  ARFMdecpA19c                                                                20210116124042  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20210116034100  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20210116034101  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20210116034101  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20210116034101  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20210116034101  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20210116034101  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20210116034101  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20210116034102  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20210116034102                      G�O�G�O�G�O�                JA  ARUP                                                                        20210116035210                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20210116153735  CV  JULD            G�O�G�O�Fʷ�                JM  ARCAJMQC2.0                                                                 20210120153403  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20210120153403  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 CTD2019V1                                                       20210121152658  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20230623082710  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20230705041505                      G�O�G�O�G�O�                