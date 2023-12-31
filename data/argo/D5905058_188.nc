CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2019-11-11T12:37:43Z creation;2019-11-11T12:37:48Z conversion to V3.1;2023-06-29T05:50:43Z update;     
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
resolution        =���   axis      Z        \  :   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I`   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     \  M8   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     \  `l   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     \  s�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     \  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �0   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     \  �   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �d   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     \  �<   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     \  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     \  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �P   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �`   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �d   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �h   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �l   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �p   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20191111123743  20230705031506  5905058                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0675_188                     2C  D   NAVIS_A                         0675                            ARGO 102115                     863 @��JU�1   @���-� @7JQ�_�b�R�<61   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C�C �C"  C$  C&  C(  C*  C,  C.  C0  C2�C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'fD'� D(  D(� D(��D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DE��DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� DkfDk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dw��Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D��D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D��3D�	�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@�z�A
=qA*=qAJ=qAj=qA��A��A��A��A��A��A��A��B�\B
�\B�\B�\B"�\B*�\B2�\B:�\BB�\BJ�\BR�\BZ�\Bb�\Bj�\Br�\Bz�\B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�C ��C��C��C��C��C
��C��C��C��C��C��C��C��C��C��C�qC �qC"��C$��C&��C(��C*��C,��C.��C0��C2�qC4��C6��C8��C:��C<��C>��C@��CB��CD��CF��CH��CJ��CL��CN��CP��CR��CT��CV��CX��CZ��C\��C^��C`��Cb��Cd��Cf��Ch��Cj��Cl��Cn��Cp��Cr��Ct��Cv��Cx��Cz��C|��C~��C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�EC�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�^�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�C�Q�D (�D ��D(�D��D(�D��D(�D��D(�D��D(�D��D(�D��D(�D��D(�D��D	(�D	��D
(�D
��D(�D��D(�D��D(�D��D(�D��D(�D��D(�D��D(�D��D(�D��D(�D��D(�D��D(�D��D(�D��D(�D��D(�D��D(�D��D(�D��D(�D��D(�D��D(�D��D(�D��D(�D��D (�D ��D!(�D!��D"(�D"��D#(�D#��D$(�D$��D%(�D%��D&(�D&��D'/\D'��D((�D(��D)"�D)��D*(�D*��D+(�D+��D,(�D,��D-(�D-��D.(�D.��D/(�D/��D0(�D0��D1(�D1��D2(�D2��D3(�D3��D4(�D4��D5(�D5��D6(�D6��D7(�D7��D8(�D8��D9(�D9��D:(�D:��D;(�D;��D<(�D<��D=(�D=��D>(�D>��D?(�D?��D@(�D@��DA(�DA��DB(�DB��DC(�DC��DD(�DD��DE(�DE��DF"�DF��DG(�DG��DH(�DH��DI(�DI��DJ(�DJ��DK(�DK��DL(�DL��DM(�DM��DN(�DN��DO(�DO��DP(�DP��DQ(�DQ��DR(�DR��DS(�DS��DT(�DT��DU(�DU��DV(�DV��DW(�DW��DX(�DX��DY(�DY��DZ(�DZ��D[(�D[��D\(�D\��D](�D]��D^(�D^��D_(�D_��D`(�D`��Da(�Da��Db(�Db��Dc(�Dc��Dd(�Dd��De(�De��Df(�Df��Dg(�Dg��Dh(�Dh��Di(�Di��Dj(�Dj��Dk/\Dk��Dl(�Dl��Dm(�Dm��Dn(�Dn��Do(�Do��Dp(�Dp��Dq(�Dq��Dr(�Dr��Ds(�Ds��Dt(�Dt��Du(�Du��Dv(�Dv��Dw(�Dw��Dx"�Dx��Dy(�Dy��Dz(�Dz��D{(�D{��D|(�D|��D}(�D}��D~(�D~��D(�D��D�HD�QHD��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D��{D��{D�{D�T{D{D��{D�{D�T{DÔ{D��{D�{D�T{DĔ{D��{D�{D�T{DŔ{D��{D�{D�T{DƔ{D��{D�{D�T{Dǔ{D��{D�{D�T{DȔ{D��{D�{D�T{Dɔ{D��{D�{D�T{Dʔ{D��{D�{D�T{D˔{D��{D�{D�T{D̔{D��{D�{D�T{D͔{D��{D�{D�T{DΔ{D��{D�{D�T{Dϔ{D��{D�{D�T{DД{D��{D�{D�T{Dє{D��{D�{D�T{DҔ{D��{D�{D�T{DӔ{D��{D�{D�T{DԔ{D��{D�{D�T{DՔ{D��{D�{D�T{D֔{D��{D�{D�T{Dה{D��{D�{D�T{Dؔ{D��{D�{D�T{Dٔ{D��{D�{D�T{Dڔ{D��{D�{D�T{D۔{D��{D�{D�T{Dܔ{D��{D�{D�T{Dݔ{D��{D�{D�T{Dޔ{D��{D�{D�T{Dߔ{D��{D�{D�T{D��{D��{D�{D�T{D�{D��{D�{D�T{D�{D��{D�{D�T{D�{D��{D�{D�T{D�{D��{D�{D�T{D�{D��{D�{D�T{D�{D��{D�{D�T{D�{D��{D�{D�T{D�{D��{D�{D�T{D�{D��{D�{D�T{D�{D��{D�{D�T{D�{D��{D�{D�T{D�{D��{D�{D�T{D�{D��{D�{D�T{D�{D��{D�{D�T{D�{D��{D�{D�T{D�{D��{D�{D�T{D�{D��{D�{D�T{D�{D��{D�{D�T{D�{D��{D�{D�T{D��{D��{D�{D�T{D��{D�׮D�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A���A��A��A��A��
A�+A�  A��yA��A��mA��;A��A��
A���A���A���A��A��
A��
A��
A��
A��A���Aѝ�AуA�=qA��/A�+AͶFA�+A�5?AuA�dZA��uA���A�\)A�|�A�7LA�A�A��A���A�ĜA��A���A�oA�l�A���A��/A�  A��PA�1'A��A���A��RA�XA��A�I�A�A�A��+A�S�A�hsA��A�Q�A�G�A��A�M�A�$�A��A��;A�`BA�1A�?}A�dZA���A���A��
A�(�A��-A�A���A���A��7A�x�A��yA���A��HA�?}A��A�A���A�t�A��A�(�A�$�A�z�A��7A�G�A���A�C�A�"�A�;dA�?}A��!A�t�A�A�A~1A|Ax�9Au�As\)Ap{AmhsAm7LAl^5Aj�jAh  Ae�^Ac��A_A\5?AXffAT��ASx�AP�AO"�AM?}AI/AF1AD9XACG�AA�A?��A>~�A=C�A;��A:E�A8�A7G�A69XA5`BA4ZA3l�A1%A/�-A.ĜA.{A-33A,�!A, �A+�A*��A*VA)+A(��A(5?A'ƨA%��A%��A$ĜA#ƨA"�uA!|�A �jA 9XAx�A�DA�^A"�A�A�-AK�AM�A�`A�#A��A��At�A�A�A�A1A7LAI�A
=A�hA
�A
I�A	�A	��A	�A
�uAbA�jA�^A�RA��A��A ��@�K�@���@�G�@��!@�G�@���@��#@��9@���@���@�@�~�@�V@�l�@�^5@�Ĝ@�5?@��T@�9@�(�@�@���@�D@�@���@�t�@��@�Ĝ@�!@�@�@���@��;@�~�@��@�j@�^5@�x�@�r�@ۥ�@�S�@ڰ!@�x�@ؓu@��
@�E�@ա�@�%@Դ9@ԃ@�z�@�1'@�|�@��@У�@ϕ�@��@��@�|�@��@��@�V@��@�z�@��;@�o@�ȴ@�v�@��/@�z�@�z�@�I�@�j@�z�@�Ĝ@�V@�`B@�r�@�"�@��
@�Q�@�(�@�bN@��
@�S�@�@�@��H@��@���@���@��D@�b@��
@���@�@�^5@��-@��@��@��H@��@�1@��F@��@�n�@���@���@�n�@�C�@��y@��\@��+@��!@��R@�ff@�@�I�@��@��
@���@��h@��7@��`@���@��@�C�@�~�@��@���@��7@��7@�/@��`@���@���@��@�b@���@��P@�;d@��@���@�=q@���@�@�p�@�G�@�/@��`@�A�@��w@���@�\)@�C�@�+@��@��H@���@���@�E�@�{@���@���@��@�%@���@��@�9X@��@�  @��F@��@�o@�o@��H@��@�V@���@�1'@�"�@���@��@�x�@�%@���@��@��/@�I�@��@���@�@���@��@�bN@�b@�1'@�1'@�
=@�-@��^@�V@��`@�Ĝ@���@�bN@�9X@�1@��m@���@��P@��@�=q@��@��@�9X@���@���@�\)@��@�@��@�v�@��T@���@��@��j@�j@��@��m@���@��@�t�@�@���@�=q@��@��-@���@�`B@�?}@��9@��u@��D@��@�z�@�j@�bN@�Q�@� �@���@��F@��@�@���@��R@���@���@��R@�~�@�5?@�@��#@���@��7@�hs@�G�@��@��u@�Q�@�b@��
@�ƨ@�ƨ@���@�l�@�\)@�C�@�"�@�@��H@���@��!@�~�@�M�@�$�@�@��T@���@�p�@�hs@�O�@�?}@�V@���@�bN@� �@�@�@~�y@~v�@}��@}�h@}p�@}O�@}?}@}/@}�@|��@|�/@|��@|I�@|1@{�m@{t�@{"�@z�H@z�\@z~�@z^5@zJ@y�#@y��@yX@y%@xĜ@x�u@xbN@xbN@xA�@w��@w\)@w�@vȴ@u�@u�h@u?}@t��@t�@tZ@s��@sƨ@s��@s��@sC�@s@r��@r-@q��@qhs@q%@p��@p�@pQ�@pA�@o�;@o�P@o;d@n�@nff@m�-@m`B@m/@l��@l�D@l(�@k��@kC�@j��@j=q@i��@h��@h �@h  @g�;@g�P@g�@fv�@e��@ep�@eO�@d��@c��@c33@b��@b-@a�@a��@`�`@`b@_��@_\)@_
=@^ȴ@^�R@^�R@^V@]��@]/@\�j@\j@\1@[�@[�@[dZ@[o@Z�H@Z~�@Z�@ZJ@Y��@Y&�@X��@Xr�@W�w@W+@V�R@Vff@V5?@V5?@V5?@V5?@V$�@U��@U?}@T�@T�@S�m@Sƨ@S��@S�@St�@SdZ@S"�@R��@R��@RM�@Q��@Qhs@Q&�@Q%@P��@PQ�@P �@O�@Ol�@N�@N�+@N5?@N$�@N{@N@M��@M@M�h@M?}@L�@L��@L��@L�D@L�D@Lz�@LI�@L(�@L1@Kƨ@K�F@K��@K�@K"�@J��@J^5@J-@I�@I�^@I��@IG�@H�`@H�9@H�9@H�@H  @G�w@Gl�@F�y@F$�@E�h@Ep�@D��@D(�@C�m@C�@CS�@Co@B�@B��@B~�@B-@A�@Ax�@A�@@��@@�@@bN@@ �@?�@?�;@?�w@?��@?l�@>�@>��@>V@>5?@=@=?}@<��@<��@<j@<9X@;�m@;��@;t�@;S�@;C�@;"�@:�H@:��@:�\@:~�@:M�@9��@9�#@9�^@9�7@9hs@9G�@8�`@8�@8 �@7�;@7�;@7�w@7�@7�P@7\)@7
=@6�R@6ff@5�@5�T@5�h@5O�@4��@4�@4��@4�@4z�@4j@49X@4�@3��@3�m@3��@3�@3C�@3@2�H@2��@2n�@2-@1��@1��@1��@1G�@1&�@1�@0��@0�u@0bN@01'@0 �@0b@/��@/�P@/l�@/\)@/;d@.��@.�y@.�@.ȴ@.��@.v�@.@-�@-�-@-?}@,�/@,��@,j@,Z@,�@+��@+t�@+"�@*�!@*�\@*M�@*=q@*�@)��@)�#@)�^@)x�@)%@(Ĝ@(�u@(bN@(A�@(b@'�P@'l�@';d@&�y@&�+@&v�@&ff@%�@%��@%�@$��@$j@$Z@$9X@$�@#�F@#��@#33@"�H@"��@"�\@"n�@"=q@!��@!G�@!&�@!�@!%@ �`@ Ĝ@ �9@ A�@   @�@�@��@�w@��@�P@|�@+@�y@ȴ@ȴ@ȴ@��@v�@5?@{@�T@@��@?}@�@�j@��@z�@I�@9X@1@ƨ@�@t�@dZ@"�@@��@�\@^5@M�@�@�@��@��@hs@G�@�@�`@�9@Q�@1'@ �@  @�;@��@l�@\)@+@ȴ@��@E�@$�@@�T@��@�@?}@�@�@��@z�@j@I�@(�@1@�m@ƨ@�@dZ@�@��@~�@^5@-@�#@�^@x�@hs@X@7L@%@�`@Ĝ@Q�@ �@�@�w@��@l�@+@�y@��@v�@ff@E�@{@�T@�-@�h@�@p�@/@�@�@V@��@�@�D@z�@j@I�@(�@1@��@ƨ@�@S�@o@@
�@
�@
��@
��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A���A��A��A��A��
A�+A�  A��yA��A��mA��;A��A��
A���A���A���A��A��
A��
A��
A��
A��A���Aѝ�AуA�=qA��/A�+AͶFA�+A�5?AuA�dZA��uA���A�\)A�|�A�7LA�A�A��A���A�ĜA��A���A�oA�l�A���A��/A�  A��PA�1'A��A���A��RA�XA��A�I�A�A�A��+A�S�A�hsA��A�Q�A�G�A��A�M�A�$�A��A��;A�`BA�1A�?}A�dZA���A���A��
A�(�A��-A�A���A���A��7A�x�A��yA���A��HA�?}A��A�A���A�t�A��A�(�A�$�A�z�A��7A�G�A���A�C�A�"�A�;dA�?}A��!A�t�A�A�A~1A|Ax�9Au�As\)Ap{AmhsAm7LAl^5Aj�jAh  Ae�^Ac��A_A\5?AXffAT��ASx�AP�AO"�AM?}AI/AF1AD9XACG�AA�A?��A>~�A=C�A;��A:E�A8�A7G�A69XA5`BA4ZA3l�A1%A/�-A.ĜA.{A-33A,�!A, �A+�A*��A*VA)+A(��A(5?A'ƨA%��A%��A$ĜA#ƨA"�uA!|�A �jA 9XAx�A�DA�^A"�A�A�-AK�AM�A�`A�#A��A��At�A�A�A�A1A7LAI�A
=A�hA
�A
I�A	�A	��A	�A
�uAbA�jA�^A�RA��A��A ��@�K�@���@�G�@��!@�G�@���@��#@��9@���@���@�@�~�@�V@�l�@�^5@�Ĝ@�5?@��T@�9@�(�@�@���@�D@�@���@�t�@��@�Ĝ@�!@�@�@���@��;@�~�@��@�j@�^5@�x�@�r�@ۥ�@�S�@ڰ!@�x�@ؓu@��
@�E�@ա�@�%@Դ9@ԃ@�z�@�1'@�|�@��@У�@ϕ�@��@��@�|�@��@��@�V@��@�z�@��;@�o@�ȴ@�v�@��/@�z�@�z�@�I�@�j@�z�@�Ĝ@�V@�`B@�r�@�"�@��
@�Q�@�(�@�bN@��
@�S�@�@�@��H@��@���@���@��D@�b@��
@���@�@�^5@��-@��@��@��H@��@�1@��F@��@�n�@���@���@�n�@�C�@��y@��\@��+@��!@��R@�ff@�@�I�@��@��
@���@��h@��7@��`@���@��@�C�@�~�@��@���@��7@��7@�/@��`@���@���@��@�b@���@��P@�;d@��@���@�=q@���@�@�p�@�G�@�/@��`@�A�@��w@���@�\)@�C�@�+@��@��H@���@���@�E�@�{@���@���@��@�%@���@��@�9X@��@�  @��F@��@�o@�o@��H@��@�V@���@�1'@�"�@���@��@�x�@�%@���@��@��/@�I�@��@���@�@���@��@�bN@�b@�1'@�1'@�
=@�-@��^@�V@��`@�Ĝ@���@�bN@�9X@�1@��m@���@��P@��@�=q@��@��@�9X@���@���@�\)@��@�@��@�v�@��T@���@��@��j@�j@��@��m@���@��@�t�@�@���@�=q@��@��-@���@�`B@�?}@��9@��u@��D@��@�z�@�j@�bN@�Q�@� �@���@��F@��@�@���@��R@���@���@��R@�~�@�5?@�@��#@���@��7@�hs@�G�@��@��u@�Q�@�b@��
@�ƨ@�ƨ@���@�l�@�\)@�C�@�"�@�@��H@���@��!@�~�@�M�@�$�@�@��T@���@�p�@�hs@�O�@�?}@�V@���@�bN@� �@�@�@~�y@~v�@}��@}�h@}p�@}O�@}?}@}/@}�@|��@|�/@|��@|I�@|1@{�m@{t�@{"�@z�H@z�\@z~�@z^5@zJ@y�#@y��@yX@y%@xĜ@x�u@xbN@xbN@xA�@w��@w\)@w�@vȴ@u�@u�h@u?}@t��@t�@tZ@s��@sƨ@s��@s��@sC�@s@r��@r-@q��@qhs@q%@p��@p�@pQ�@pA�@o�;@o�P@o;d@n�@nff@m�-@m`B@m/@l��@l�D@l(�@k��@kC�@j��@j=q@i��@h��@h �@h  @g�;@g�P@g�@fv�@e��@ep�@eO�@d��@c��@c33@b��@b-@a�@a��@`�`@`b@_��@_\)@_
=@^ȴ@^�R@^�R@^V@]��@]/@\�j@\j@\1@[�@[�@[dZ@[o@Z�H@Z~�@Z�@ZJ@Y��@Y&�@X��@Xr�@W�w@W+@V�R@Vff@V5?@V5?@V5?@V5?@V$�@U��@U?}@T�@T�@S�m@Sƨ@S��@S�@St�@SdZ@S"�@R��@R��@RM�@Q��@Qhs@Q&�@Q%@P��@PQ�@P �@O�@Ol�@N�@N�+@N5?@N$�@N{@N@M��@M@M�h@M?}@L�@L��@L��@L�D@L�D@Lz�@LI�@L(�@L1@Kƨ@K�F@K��@K�@K"�@J��@J^5@J-@I�@I�^@I��@IG�@H�`@H�9@H�9@H�@H  @G�w@Gl�@F�y@F$�@E�h@Ep�@D��@D(�@C�m@C�@CS�@Co@B�@B��@B~�@B-@A�@Ax�@A�@@��@@�@@bN@@ �@?�@?�;@?�w@?��@?l�@>�@>��@>V@>5?@=@=?}@<��@<��@<j@<9X@;�m@;��@;t�@;S�@;C�@;"�@:�H@:��@:�\@:~�@:M�@9��@9�#@9�^@9�7@9hs@9G�@8�`@8�@8 �@7�;@7�;@7�w@7�@7�P@7\)@7
=@6�R@6ff@5�@5�T@5�h@5O�@4��@4�@4��@4�@4z�@4j@49X@4�@3��@3�m@3��@3�@3C�@3@2�H@2��@2n�@2-@1��@1��@1��@1G�@1&�@1�@0��@0�u@0bN@01'@0 �@0b@/��@/�P@/l�@/\)@/;d@.��@.�y@.�@.ȴ@.��@.v�@.@-�@-�-@-?}@,�/@,��@,j@,Z@,�@+��@+t�@+"�@*�!@*�\@*M�@*=q@*�@)��@)�#@)�^@)x�@)%@(Ĝ@(�u@(bN@(A�@(b@'�P@'l�@';d@&�y@&�+@&v�@&ff@%�@%��@%�@$��@$j@$Z@$9X@$�@#�F@#��@#33@"�H@"��@"�\@"n�@"=q@!��@!G�@!&�@!�@!%@ �`@ Ĝ@ �9@ A�@   @�@�@��@�w@��@�P@|�@+@�y@ȴ@ȴ@ȴ@��@v�@5?@{@�T@@��@?}@�@�j@��@z�@I�@9X@1@ƨ@�@t�@dZ@"�@@��@�\@^5@M�@�@�@��@��@hs@G�@�@�`@�9@Q�@1'@ �@  @�;@��@l�@\)@+@ȴ@��@E�@$�@@�T@��@�@?}@�@�@��@z�@j@I�@(�@1@�m@ƨ@�@dZ@�@��@~�@^5@-@�#@�^@x�@hs@X@7L@%@�`@Ĝ@Q�@ �@�@�w@��@l�@+@�y@��@v�@ff@E�@{@�T@�-@�h@�@p�@/@�@�@V@��@�@�D@z�@j@I�@(�@1@��@ƨ@�@S�@o@@
�@
�@
��@
��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B
1'B
1'B
0!B
1'B
33B
=qB
>wB
=qB
?}B
?}B
?}B
=qB
?}B
?}B
@�B
@�B
B�B
C�B
D�B
F�B
F�B
G�B
O�B
aHB
|�B
�%B
�+B
�{B
�B
�yB49BI�BL�B=qB>wB�B%�B6FBI�B�VBĜB�^BÖB�HB��B�FB��BDB49B+B.B,BC�BQ�BW
BZBiyBn�BiyBk�B^5BM�BA�B33B!�BhB��B�B�)B��B�HB��B�!BɺB��BB�BB�
B�LB��B�?B��B�uB�Bx�B�%B�%B��B��Bm�BdZBS�B6FB(�B�B	7B
�B
�B
�mB
��B
�^B
��B
�B
t�B
dZB
J�B
33B
�B
%B	�fB	��B	�wB	��B	��B	�{B	�+B	r�B	\)B	N�B	5?B	�B	1B��B�fB�#B��BǮB�FB��B��B�{B�VB�1B�%B�Bz�Bv�Bm�BgmBgmBe`BbNBaHB_;B_;B[#BYBYBYBXBYBaHBbNBcTBbNBaHBaHB_;B\)B[#BYBYBYBYBYB\)BbNB`BB^5BYBT�BQ�BM�BK�BF�BD�BA�B@�BK�BJ�BF�BE�BE�BG�BA�B9XB8RB8RB:^B;dBD�BcTBXBK�BG�BI�BM�BE�B33B.B.B,B&�B'�B)�B-B0!B33B5?B5?B=qB@�BJ�BK�B9XBB�BM�BL�BK�BK�BO�BVBhsBs�Bv�B�B�Bz�Bz�By�B�=B�PB�JB�DB�\B�=B�1B�1B�1B�7B�=B�7B�1B�7B�uB�{B�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�9B�B�B�-B�3B�?B�FB�jBŢB��B��BȴB��B��B��B�B�#B�#B�/B�5B�NB�B�B�B�B��B��B��B��B��B��B	B	B	B��B	B	%B	DB	PB	oB	�B	"�B	,B	/B	/B	1'B	8RB	;dB	=qB	>wB	=qB	D�B	C�B	B�B	A�B	A�B	B�B	B�B	D�B	C�B	F�B	H�B	K�B	K�B	L�B	P�B	R�B	R�B	R�B	S�B	[#B	]/B	_;B	bNB	e`B	gmB	jB	k�B	l�B	o�B	p�B	q�B	r�B	w�B	x�B	y�B	z�B	|�B	~�B	~�B	~�B	~�B	� B	�B	�B	�B	�B	�+B	�+B	�7B	�JB	�PB	�PB	�PB	�VB	�\B	�bB	�hB	�uB	�oB	�\B	�PB	�DB	�=B	�JB	�JB	�VB	�\B	�bB	�bB	�oB	�uB	�{B	��B	�{B	�oB	�oB	�hB	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�!B	�-B	�?B	�RB	�^B	�^B	�dB	�jB	�jB	�jB	�qB	�wB	��B	ÖB	ĜB	ĜB	ŢB	ƨB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�
B	�B	�#B	�/B	�/B	�5B	�5B	�5B	�5B	�;B	�BB	�NB	�TB	�ZB	�ZB	�ZB	�ZB	�`B	�fB	�fB	�fB	�mB	�mB	�sB	�sB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
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
%B
%B
+B
+B
+B
1B
1B
1B
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
PB
VB
VB
VB
\B
bB
hB
oB
oB
oB
oB
uB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
"�B
"�B
"�B
#�B
$�B
$�B
$�B
#�B
#�B
$�B
$�B
$�B
$�B
$�B
$�B
$�B
%�B
%�B
%�B
%�B
&�B
'�B
'�B
'�B
'�B
(�B
(�B
(�B
(�B
(�B
(�B
)�B
)�B
(�B
(�B
)�B
)�B
)�B
)�B
+B
+B
+B
+B
+B
+B
+B
+B
,B
,B
,B
,B
-B
-B
-B
-B
-B
.B
.B
.B
.B
/B
/B
/B
/B
0!B
0!B
0!B
1'B
1'B
2-B
2-B
2-B
33B
33B
49B
49B
49B
49B
49B
5?B
5?B
6FB
7LB
7LB
8RB
8RB
8RB
8RB
8RB
8RB
8RB
8RB
9XB
9XB
:^B
:^B
:^B
:^B
;dB
;dB
;dB
;dB
;dB
<jB
<jB
<jB
<jB
<jB
<jB
=qB
=qB
=qB
=qB
>wB
>wB
>wB
>wB
>wB
>wB
?}B
?}B
?}B
@�B
@�B
@�B
@�B
@�B
A�B
A�B
A�B
A�B
B�B
B�B
B�B
C�B
C�B
C�B
C�B
C�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
E�B
E�B
E�B
E�B
F�B
F�B
F�B
F�B
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
I�B
I�B
I�B
I�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
K�B
K�B
L�B
L�B
L�B
L�B
M�B
M�B
M�B
M�B
N�B
N�B
N�B
N�B
N�B
N�B
O�B
O�B
O�B
O�B
P�B
P�B
P�B
P�B
P�B
Q�B
Q�B
Q�B
Q�B
R�B
R�B
Q�B
R�B
R�B
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
T�B
T�B
T�B
VB
VB
VB
VB
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
XB
XB
XB
XB
XB
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
ZB
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
^5B
^5B
^5B
^5B
^5B
^5B
_;B
_;B
_;B
_;B
_;B
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
bNB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
cTB
cTB
cTB
cTB
cTB
cTB
cTB
cTB
cTB
dZB
dZB
dZB
dZB
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
hsB
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
jB
jB
jB
jB
jB
k�B
jB
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
l�B
l�B
l�B
l�B
l�B
m�B
m�B
m�B
m�B
m�B
m�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B
1B
1B
0!B
1vB
49B
=qB
>]B
=<B
?cB
?cB
?HB
=VB
?HB
?HB
@OB
@OB
B[B
CaB
DgB
FtB
F�B
G�B
P.B
a�B
}qB
�_B
��B
�7B
�#B
��B:�BNBP�BA�BB�B!|B(>B7�BIRB��B��B��BňB�B�B�8B��B�B4�B,qB0�B-)BDBR�BXyB]dBkkBoBjBm�B`\BP�BCB5tB$&B[B OB�BݘB��B�B��B�oB�=B��BªB�|B��B��B��B�LB��B�2B��Bx�B��B�tB��B��Bp�BgRBV�B8�B+�B!�B
�B
�B
�/B
�B
��B
�B
��B
��B
w�B
gB
M�B
5�B
"�B
	�B	��B	�YB	��B	�|B	��B	��B	�XB	utB	_pB	S[B	9�B	 �B	�B��B�yB�B��B�B��B��B��B�mB�.B��B��B��B|�Bx�Bo Bh�BhsBf�Bc�Bc�B`�B`'B[�BY�BY�BY�BX�BY�Ba�BcnBc�Bb�BbBcB_�B]/B\CBZ�BZ7BY�BY�BY�B]IBcB`�B_!BY�BU�BS�BQ4BMBG�BEmBBBA�BL�BK�BG�BF�BF�BIBB�B:B8�B8�B:^B;BD�Be�BYBMBH�BJXBP.BG�B4nB.}B/B-]B'�B(�B*�B-�B0�B3�B5tB4�B=<B@4BK�BM�B8�BB�BNBL�BK�BKDBOBBU�Bh�BtTBwB�{B��B{dB{0By�B��B��B��B�B�.B��B��B��B�KB��B��B��B��B�	B��B��B�{B��B��B��B�@B��B�zB�TB�|B�jB�dB�QB��B�sB�B��B�B��B��B�DB��B�B��B��B��B��B��B�B�mB�B�B�B�VBҽB��B�+B�=B�#B�B��B�B� B��B�B�B��B��B�B�B�<B�HB	oB	�B	�B��B	B	YB	^B	PB	 B	�B	"4B	+�B	/ B	.�B	0�B	8B	;B	=�B	>�B	="B	EB	C�B	B�B	AoB	A�B	BuB	B�B	D�B	C�B	F�B	H�B	K�B	K�B	L�B	P�B	R�B	R�B	R�B	TB	[	B	\�B	_;B	bNB	eFB	gmB	jeB	kkB	lqB	o�B	poB	q�B	r�B	w�B	x�B	y�B	z�B	|�B	~�B	~�B	~�B	~�B	�B	��B	��B	��B	�B	��B	�B	�B	�0B	�B	�6B	�PB	�pB	�(B	�B	�hB	��B	��B	�\B	��B	��B	�rB	�dB	�JB	�<B	�(B	�.B	�.B	��B	�uB	��B	�B	��B	��B	�:B	�NB	�B	�uB	��B	��B	��B	��B	�?B	�YB	�EB	�_B	�kB	�kB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�!B	�GB	�?B	�RB	�DB	�DB	�JB	�B	�6B	�PB	�VB	�]B	�oB	�{B	�gB	āB	�mB	ƨB	ɆB	�rB	�xB	�xB	˒B	˒B	�~B	͹B	οB	бB	ѷB	�B	��B	՛B	ּB	ּB	��B	��B	��B	��B	�B	�B	�B	�B	�B	�!B	�BB	�4B	� B	�B	�&B	�&B	�&B	�,B	�2B	�2B	�2B	�8B	�8B	�>B	�$B	�XB	�DB	�DB	�KB	�KB	�eB	�QB	�=B	�WB	�WB	�qB	�qB	�}B	�cB	�B	�B	�aB	�|B	�B	�B	�nB	�B	�B	�B	�nB	�B	�B	�tB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
B
�B
�B
�B
�B
�B
�B
B
	B
	B
	B

#B

#B
B
B
B
0B
6B
B
<B
<B
VB
BB
bB
hB
:B
:B
TB
TB
[B
@B
aB
MB
gB
�B
�B
YB
_B
_B
yB
yB
�B
QB
�B
qB
WB
qB
qB
�B
�B
�B
~B
~B
�B
�B
jB
�B
�B
jB
�B
pB
�B
�B
 �B
 �B
!�B
!�B
!�B
!�B
"�B
"�B
"�B
"�B
#�B
$�B
$�B
$�B
#�B
#�B
$�B
$�B
$�B
$tB
$�B
$�B
$�B
%�B
%�B
%�B
%�B
&�B
'�B
'�B
'�B
'�B
(�B
(�B
(�B
(�B
(�B
(�B
)�B
)�B
(�B
(�B
)�B
)�B
)�B
)�B
*�B
*�B
*�B
*�B
*�B
*�B
*�B
*�B
+�B
+�B
+�B
+�B
,�B
,�B
,�B
,�B
,�B
-�B
-�B
-�B
-�B
.�B
.�B
.�B
/ B
/�B
/�B
0B
1B
0�B
1�B
2B
1�B
2�B
3B
4B
4B
4B
4B
4B
4�B
5B
6B
7B
7B
8B
8B
8B
8B
8B
8B
8B
8B
9>B
9	B
:*B
:*B
:DB
:DB
;B
;0B
;0B
;B
;B
<6B
<B
<6B
<B
<6B
<6B
=<B
=<B
=<B
=<B
>BB
>(B
>BB
>BB
>BB
>BB
?HB
?cB
?.B
@4B
@4B
@OB
@OB
@OB
A;B
A;B
AUB
A;B
B[B
B[B
B[B
CGB
CaB
CaB
CaB
CGB
DMB
DMB
DgB
DgB
DgB
DgB
DgB
EmB
ESB
EmB
EmB
FtB
FtB
FtB
FYB
GzB
GzB
GzB
G_B
GzB
H�B
H�B
HfB
HfB
HfB
I�B
I�B
I�B
I�B
I�B
IlB
I�B
JrB
JrB
J�B
J�B
J�B
J�B
J�B
JrB
K�B
K�B
L�B
L�B
L�B
L�B
M�B
M�B
M�B
M�B
N�B
N�B
N�B
N�B
N�B
N�B
O�B
O�B
O�B
O�B
P�B
P�B
P�B
P�B
P�B
Q�B
Q�B
Q�B
Q�B
R�B
R�B
Q�B
R�B
R�B
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
T�B
T�B
T�B
U�B
U�B
U�B
U�B
U�B
U�B
U�B
U�B
V�B
V�B
V�B
V�B
W�B
W�B
W�B
W�B
W�B
X�B
X�B
X�B
X�B
X�B
Y�B
Y�B
Y�B
Y�B
Z�B
Y�B
Z�B
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
^B
]�B
^B
^B
^B
^B
_B
_B
^�B
^�B
^�B
_�B
`B
_�B
_�B
`B
aB
aB
aB
`�B
`�B
a�B
bB
bB
bB
a�B
bB
a�B
bB
cB
c B
c B
cB
cB
c B
c B
c B
c B
dB
d&B
dB
d@B
e,B
e,B
eB
e,B
f2B
f2B
fB
fB
fB
fB
f2B
f2B
f2B
f2B
g8B
h>B
h>B
h>B
h>B
h>B
h$B
iDB
i*B
i*B
i*B
iDB
iDB
iDB
jKB
j0B
jKB
j0B
j0B
k6B
jKB
kQB
kQB
k6B
k6B
kB
kQB
k6B
kQB
l=B
lWB
lWB
l=B
l=B
mCB
mCB
m)B
mCB
m)B
mC11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<5�U<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=-0.64(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201911170035182019111700351820191117003518202306231718552023062317185520230623171855201911180026142019111800261420191118002614  JA  ARFMdecpA19c                                                                20191111213710  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20191111123743  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20191111123745  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20191111123746  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20191111123746  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20191111123746  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20191111123746  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20191111123746  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20191111123748  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20191111123748                      G�O�G�O�G�O�                JA  ARUP                                                                        20191111125600                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20191111153639  CV  JULD            G�O�G�O�F�X�                JM  ARCAJMQC2.0                                                                 20191116153518  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20191116153518  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20191117152614  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20230623081855  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20230705031506                      G�O�G�O�G�O�                