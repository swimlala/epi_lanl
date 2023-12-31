CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2020-12-17T03:40:07Z creation;2020-12-17T03:40:09Z conversion to V3.1;2023-06-29T05:47:42Z update;     
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20201217034007  20230705041504  5905058                                                                 JAMSTEC                                                         PRES            TEMP            PSAL              A   JA  I2_0675_282                     2C  D   NAVIS_A                         0675                            ARGO 102115                     863 @�Oy��5�1   @�Oz�`�@6[���A�b�%F
�L1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @333@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B ffB(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D �fDfD� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  Dy�D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD�fDfD� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DA��DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�3D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�ɚ111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @U�@���@���Az�A(z�AHz�Ahz�A�=qA�=qA�=qA�=qA�=qA�=qA�=qA�=qB�B
�B�B�B"�B*�B2�B:�BB�BJ�BR�BZ�Bb�Bj�Br�Bz�B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\C ��C��C��C��C��C
��C��C��C��C��C��C��C��C��C��C��C ��C"��C$��C&��C(��C*��C,��C.��C0��C2��C4��C6��C8��C:��C<��C>��C@��CB��CD��CF��CH��CJ��CL��CN��CP��CR��CT��CV��CX��CZ��C\��C^��C`��Cb��Cd��Cf��Ch��Cj��Cl��Cn��Cp��Cr��Ct��Cv��Cx��Cz��C|��C~��C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�D !�D �RD(RD��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D	!�D	��D
!�D
��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D(RD�RD(RD��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D !�D ��D!!�D!��D"!�D"��D#!�D#��D$!�D$��D%!�D%��D&!�D&��D'!�D'��D(!�D(��D)!�D)��D*!�D*��D+!�D+��D,!�D,��D-!�D-��D.!�D.��D/!�D/��D0!�D0��D1!�D1��D2!�D2��D3!�D3��D4!�D4��D5!�D5��D6!�D6��D7!�D7��D8!�D8��D9!�D9��D:!�D:��D;!�D;��D<!�D<��D=!�D=��D>!�D>��D?!�D?��D@!�D@��DA!�DA��DB�DB��DC!�DC��DD!�DD��DE!�DE��DF!�DF��DG!�DG��DH!�DH��DI!�DI��DJ!�DJ��DK!�DK��DL!�DL��DM!�DM��DN!�DN��DO!�DO��DP!�DP��DQ!�DQ��DR!�DR��DS!�DS��DT!�DT��DU!�DU��DV!�DV��DW!�DW��DX!�DX��DY!�DY��DZ!�DZ��D[!�D[��D\!�D\��D]!�D]��D^!�D^��D_!�D_��D`!�D`��Da!�Da��Db!�Db��Dc!�Dc��Dd!�Dd��De!�De��Df!�Df��Dg!�Dg��Dh!�Dh��Di!�Di��Dj!�Dj��Dk!�Dk��Dl!�Dl��Dm!�Dm��Dn!�Dn��Do!�Do��Dp!�Dp��Dq!�Dq��Dr!�Dr��Ds!�Ds��Dt!�Dt��Du!�Du��Dv!�Dv��Dw!�Dw��Dx!�Dx��Dy!�Dy��Dz!�Dz��D{!�D{��D|!�D|��D}!�D}��D~!�D~��D!�D��D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D�D���D��D�P�DÐ�D���D��D�P�DĐ�D���D��D�P�DŐ�D���D��D�P�DƐ�D���D��D�P�Dǐ�D���D��D�P�DȐ�D���D��D�P�Dɐ�D���D��D�P�Dʐ�D���D��D�P�Dː�D���D��D�P�D̐�D���D��D�P�D͐�D���D��D�P�Dΐ�D���D��D�P�Dϐ�D���D��D�P�DА�D���D��D�P�Dѐ�D���D��D�P�DҐ�D���D��D�P�DӐ�D���D��D�P�DԐ�D���D��D�P�DՐ�D���D��D�P�D֐�D���D��D�P�Dא�D���D��D�P�Dؐ�D���D��D�P�Dِ�D���D��D�P�Dڐ�D���D��D�P�Dې�D���D��D�P�Dܐ�D���D��D�P�Dݐ�D���D��D�P�Dސ�D���D��D�P�Dߐ�D���D��D�P�D���D���D��D�P�D��D���D��D�P�D��D���D��D�P�D��D���D��D�P�D��D���D��D�P�D��D���D��D�P�D��D���D��D�P�D��D���D��D�P�D��D���D��D�P�D��D���D��D�P�D��D���D��D�P�D��D���D��D�P�D��D���D��D�P�D��D���D��D�P�D��D���D��D�P�D�)D���D��D�P�D��D���D��D�P�D��D���D��D�P�D��D���D��D�P�D��D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D�ڏ111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�G�A�G�A�G�A�I�A�G�A�G�A�G�A�I�A�G�A�I�A�A�A�E�A�?}A�E�A�Q�A�`BAƁAƉ7Aƙ�AƝ�Aƕ�AƋDA�|�A�l�A�\)A�;dA�5?A�7LA�5?A��A�JA��A�
=A��A��Aŝ�A��A��A�\)A��A�A�A��jA��jA�K�A�5?A�-A�t�A��yA���A�ȴA�x�A���A�S�A���A� �A�O�A��A�XA���A�$�A�ƨA��yA���A��PA���A��A��A��A���A��`A��
A�A��A�A�+A���A�dZA�{A�VA��uA��TA���A�E�A��mA�O�A�x�A���A�jA�dZA��A�M�A��-A��/A��TA��A�x�A��wA�1A��FA���A�x�A�A}�A{�Ay`BAv-As�Arn�Aq�PAp�yAoG�Ak
=Ah$�Ag�;Af��Ad�\Ac�^Ab��A`�9A_��A^v�A\�yAZ�AWƨAW
=AUt�AS33ARbAO�PAM/AK��AJ �AGƨAD�`ABZAA��A@�/A@�A?+A=�A;��A:A�A8�A8I�A7O�A6ZA6jA6bNA5�wA4�`A3�PA3
=A2�DA1�A0��A0A�A/"�A-�wA,z�A+�A*{A)�hA)/A(��A(bNA'��A';dA&$�A%��A$ffA#�A"r�A!��A v�A�A��A��A�!AbAĜA��A%A$�A��A��AO�An�A�AXAE�AE�A|�A%A��A�RA(�A��A��A
�`A
  A5?A\)A��A"�AJAVA�FA��A�!AbNA|�A ��@��@���@���@��@���@��@���@��@�5?@���@��@�E�@�@�A�@�@�@�u@�@�=q@��@�C�@�@�@��m@�;d@��@�F@�R@�;d@�^5@��T@�O�@ܬ@�+@�O�@�%@�Q�@�dZ@�+@�{@Չ7@�`B@��/@��;@���@��@Л�@�bN@��;@�p�@��/@�(�@�+@�v�@ɉ7@ǥ�@�ff@Ų-@�hs@ģ�@�I�@�\)@��@�`B@�j@�1@��P@�+@���@��h@���@�r�@�I�@�dZ@���@���@��`@�1@��P@�C�@�{@���@���@�  @��@�"�@��!@��^@�7L@�Z@�+@�V@��^@��@�X@�bN@��@�+@�{@��h@�&�@��@�|�@�S�@���@�E�@���@�hs@�r�@�(�@�1@��
@�S�@�o@��H@�ȴ@���@���@�ff@�x�@�/@�?}@�G�@��@�r�@�A�@�z�@�Z@�  @���@���@�\)@�t�@�|�@�C�@��@�^5@�$�@�J@���@��@���@���@��@��;@���@�ȴ@�{@���@���@��7@��@���@��@��@���@�+@��y@�ȴ@���@�^5@�$�@���@�@��7@�X@���@�A�@�b@��m@���@�|�@�S�@�o@��R@���@�ff@�5?@��T@��#@���@���@�@���@��h@�x�@�`B@�?}@�V@��/@�Ĝ@��j@��9@���@�z�@�Q�@� �@�  @��P@�S�@��@��y@��H@���@���@���@���@���@��R@��!@���@�~�@�V@�-@���@�@��-@���@���@���@��h@��@�X@�O�@�G�@�/@���@��/@��j@���@��D@�r�@�A�@��;@��w@��@�\)@�"�@�@���@��\@��+@�M�@�J@���@�`B@�O�@�?}@��@��9@��9@�z�@�Q�@�(�@��@���@���@�dZ@�;d@���@�ȴ@��+@�v�@�v�@�E�@�{@���@�hs@�G�@�%@���@�Z@�1@��;@���@�dZ@�33@�@���@��R@�~�@�ff@�V@�E�@��T@�`B@�G�@�7L@��@��@�Q�@�1'@��@�1@�  @��@+@~��@~��@~{@}��@|�D@|1@|1@{�F@{S�@z��@z^5@y��@yhs@x�`@x�9@x  @wl�@vE�@u�T@u�h@t�j@tI�@s��@s��@s��@sdZ@r�\@r-@q��@q�@pr�@pA�@p  @o�w@n�@n��@m�@m��@l��@l��@l�@l��@lz�@l1@kC�@ko@j�\@i��@i��@iG�@h��@hr�@hQ�@h �@g�w@g;d@f�R@f$�@e�T@e��@e@e�-@e��@e��@e��@e�h@e�@e�@e/@d�D@d�@c�
@c�
@c�F@c�@cdZ@c33@c33@c33@c"�@co@c@b��@b~�@bM�@bM�@b-@a��@ahs@a7L@`��@`Ĝ@`r�@_��@_�P@_K�@^��@^�@^��@^V@]�@]��@]p�@]O�@]/@\�/@\��@\z�@\j@\Z@\9X@\�@[�
@[�@["�@Z�@Z�H@Z�!@Z^5@Y��@Y%@X�u@XQ�@Xb@W�@W��@W�w@V�@V�+@V5?@V@U�-@T��@Tz�@TZ@T9X@S��@S�@SS�@S33@S"�@So@R�H@R�\@R�@Q�#@Q��@Q��@QG�@Q�@PĜ@P�u@PQ�@P �@O��@O|�@O+@N�y@N��@NE�@M@MO�@MV@L�@L��@Lz�@L(�@K��@K��@KC�@K"�@K@J�!@J^5@I��@Ix�@IG�@I�@HĜ@H��@H�@H1'@G��@G|�@G+@F�@Fv�@FE�@F{@E�@E�-@EO�@E?}@E�@D�@D��@D�j@DZ@D(�@D1@C�
@C�
@Cƨ@C�F@C��@Ct�@C@B�@B�H@B�\@BM�@A�^@A7L@@��@@Ĝ@@bN@?�P@?\)@?K�@?;d@>��@>�R@>$�@=�T@=�T@=@=�-@=O�@<�@<�D@<Z@<�@;��@;t�@;C�@;33@;"�@:�@:�@:J@:J@9�@9�#@9�^@9hs@9�@8�@8A�@8 �@8  @7�;@7��@7�P@7K�@7+@7�@6�@6��@6V@6{@5@5�@5O�@5V@4��@4I�@3��@3ƨ@3�@333@3@2�@2�H@2��@2~�@2J@1�#@1��@1X@0��@0�@0bN@0A�@/�;@/��@/�P@/�P@/|�@/l�@/\)@/K�@/K�@/+@.��@.ȴ@.��@.v�@.ff@.ff@.5?@.@-�T@-��@-@-@-@-�h@-�@-/@,��@,��@,�@,z�@,Z@,1@+ƨ@+�F@+��@+��@+t�@+"�@*��@*�\@*~�@*^5@*^5@*M�@)�@)�^@)�7@)G�@)&�@(��@(��@(bN@(1'@(b@'�;@'�@'�@&�y@&�y@&�@&�@&�@&�@&ȴ@&ȴ@&��@&v�@&V@%@%p�@%�@$��@$��@$9X@#�F@#@"��@"~�@!��@!�7@ ��@ Ĝ@ r�@   @�w@��@\)@;d@+@+@+@�@�y@�+@$�@{@@@@�-@�@p�@p�@`B@`B@?}@?}@�@�@�/@��@��@�D@�@�F@��@t�@dZ@C�@C�@33@"�@o@@@�@��@��@��@^5@�@��@�#@��@�7@G�@%@Ĝ@�@1'@ �@�;@�@�P@l�@;d@��@��@v�@E�@5?@�@�@�j@z�@j@I�@(�@�
@dZ@dZ@S�@33@o@��@�\@��@��@X@7L@7L@�@%@��@�@r�@A�@�@\)@;d@�@
=@
=@�R@$�@�T@��@/@�@��@��@��@�@��@�@z�@9X@��@�m@��@@
�\@
M�@
�@	��@	�#@	��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�G�A�G�A�G�A�I�A�G�A�G�A�G�A�I�A�G�A�I�A�A�A�E�A�?}A�E�A�Q�A�`BAƁAƉ7Aƙ�AƝ�Aƕ�AƋDA�|�A�l�A�\)A�;dA�5?A�7LA�5?A��A�JA��A�
=A��A��Aŝ�A��A��A�\)A��A�A�A��jA��jA�K�A�5?A�-A�t�A��yA���A�ȴA�x�A���A�S�A���A� �A�O�A��A�XA���A�$�A�ƨA��yA���A��PA���A��A��A��A���A��`A��
A�A��A�A�+A���A�dZA�{A�VA��uA��TA���A�E�A��mA�O�A�x�A���A�jA�dZA��A�M�A��-A��/A��TA��A�x�A��wA�1A��FA���A�x�A�A}�A{�Ay`BAv-As�Arn�Aq�PAp�yAoG�Ak
=Ah$�Ag�;Af��Ad�\Ac�^Ab��A`�9A_��A^v�A\�yAZ�AWƨAW
=AUt�AS33ARbAO�PAM/AK��AJ �AGƨAD�`ABZAA��A@�/A@�A?+A=�A;��A:A�A8�A8I�A7O�A6ZA6jA6bNA5�wA4�`A3�PA3
=A2�DA1�A0��A0A�A/"�A-�wA,z�A+�A*{A)�hA)/A(��A(bNA'��A';dA&$�A%��A$ffA#�A"r�A!��A v�A�A��A��A�!AbAĜA��A%A$�A��A��AO�An�A�AXAE�AE�A|�A%A��A�RA(�A��A��A
�`A
  A5?A\)A��A"�AJAVA�FA��A�!AbNA|�A ��@��@���@���@��@���@��@���@��@�5?@���@��@�E�@�@�A�@�@�@�u@�@�=q@��@�C�@�@�@��m@�;d@��@�F@�R@�;d@�^5@��T@�O�@ܬ@�+@�O�@�%@�Q�@�dZ@�+@�{@Չ7@�`B@��/@��;@���@��@Л�@�bN@��;@�p�@��/@�(�@�+@�v�@ɉ7@ǥ�@�ff@Ų-@�hs@ģ�@�I�@�\)@��@�`B@�j@�1@��P@�+@���@��h@���@�r�@�I�@�dZ@���@���@��`@�1@��P@�C�@�{@���@���@�  @��@�"�@��!@��^@�7L@�Z@�+@�V@��^@��@�X@�bN@��@�+@�{@��h@�&�@��@�|�@�S�@���@�E�@���@�hs@�r�@�(�@�1@��
@�S�@�o@��H@�ȴ@���@���@�ff@�x�@�/@�?}@�G�@��@�r�@�A�@�z�@�Z@�  @���@���@�\)@�t�@�|�@�C�@��@�^5@�$�@�J@���@��@���@���@��@��;@���@�ȴ@�{@���@���@��7@��@���@��@��@���@�+@��y@�ȴ@���@�^5@�$�@���@�@��7@�X@���@�A�@�b@��m@���@�|�@�S�@�o@��R@���@�ff@�5?@��T@��#@���@���@�@���@��h@�x�@�`B@�?}@�V@��/@�Ĝ@��j@��9@���@�z�@�Q�@� �@�  @��P@�S�@��@��y@��H@���@���@���@���@���@��R@��!@���@�~�@�V@�-@���@�@��-@���@���@���@��h@��@�X@�O�@�G�@�/@���@��/@��j@���@��D@�r�@�A�@��;@��w@��@�\)@�"�@�@���@��\@��+@�M�@�J@���@�`B@�O�@�?}@��@��9@��9@�z�@�Q�@�(�@��@���@���@�dZ@�;d@���@�ȴ@��+@�v�@�v�@�E�@�{@���@�hs@�G�@�%@���@�Z@�1@��;@���@�dZ@�33@�@���@��R@�~�@�ff@�V@�E�@��T@�`B@�G�@�7L@��@��@�Q�@�1'@��@�1@�  @��@+@~��@~��@~{@}��@|�D@|1@|1@{�F@{S�@z��@z^5@y��@yhs@x�`@x�9@x  @wl�@vE�@u�T@u�h@t�j@tI�@s��@s��@s��@sdZ@r�\@r-@q��@q�@pr�@pA�@p  @o�w@n�@n��@m�@m��@l��@l��@l�@l��@lz�@l1@kC�@ko@j�\@i��@i��@iG�@h��@hr�@hQ�@h �@g�w@g;d@f�R@f$�@e�T@e��@e@e�-@e��@e��@e��@e�h@e�@e�@e/@d�D@d�@c�
@c�
@c�F@c�@cdZ@c33@c33@c33@c"�@co@c@b��@b~�@bM�@bM�@b-@a��@ahs@a7L@`��@`Ĝ@`r�@_��@_�P@_K�@^��@^�@^��@^V@]�@]��@]p�@]O�@]/@\�/@\��@\z�@\j@\Z@\9X@\�@[�
@[�@["�@Z�@Z�H@Z�!@Z^5@Y��@Y%@X�u@XQ�@Xb@W�@W��@W�w@V�@V�+@V5?@V@U�-@T��@Tz�@TZ@T9X@S��@S�@SS�@S33@S"�@So@R�H@R�\@R�@Q�#@Q��@Q��@QG�@Q�@PĜ@P�u@PQ�@P �@O��@O|�@O+@N�y@N��@NE�@M@MO�@MV@L�@L��@Lz�@L(�@K��@K��@KC�@K"�@K@J�!@J^5@I��@Ix�@IG�@I�@HĜ@H��@H�@H1'@G��@G|�@G+@F�@Fv�@FE�@F{@E�@E�-@EO�@E?}@E�@D�@D��@D�j@DZ@D(�@D1@C�
@C�
@Cƨ@C�F@C��@Ct�@C@B�@B�H@B�\@BM�@A�^@A7L@@��@@Ĝ@@bN@?�P@?\)@?K�@?;d@>��@>�R@>$�@=�T@=�T@=@=�-@=O�@<�@<�D@<Z@<�@;��@;t�@;C�@;33@;"�@:�@:�@:J@:J@9�@9�#@9�^@9hs@9�@8�@8A�@8 �@8  @7�;@7��@7�P@7K�@7+@7�@6�@6��@6V@6{@5@5�@5O�@5V@4��@4I�@3��@3ƨ@3�@333@3@2�@2�H@2��@2~�@2J@1�#@1��@1X@0��@0�@0bN@0A�@/�;@/��@/�P@/�P@/|�@/l�@/\)@/K�@/K�@/+@.��@.ȴ@.��@.v�@.ff@.ff@.5?@.@-�T@-��@-@-@-@-�h@-�@-/@,��@,��@,�@,z�@,Z@,1@+ƨ@+�F@+��@+��@+t�@+"�@*��@*�\@*~�@*^5@*^5@*M�@)�@)�^@)�7@)G�@)&�@(��@(��@(bN@(1'@(b@'�;@'�@'�@&�y@&�y@&�@&�@&�@&�@&ȴ@&ȴ@&��@&v�@&V@%@%p�@%�@$��@$��@$9X@#�F@#@"��@"~�@!��@!�7@ ��@ Ĝ@ r�@   @�w@��@\)@;d@+@+@+@�@�y@�+@$�@{@@@@�-@�@p�@p�@`B@`B@?}@?}@�@�@�/@��@��@�D@�@�F@��@t�@dZ@C�@C�@33@"�@o@@@�@��@��@��@^5@�@��@�#@��@�7@G�@%@Ĝ@�@1'@ �@�;@�@�P@l�@;d@��@��@v�@E�@5?@�@�@�j@z�@j@I�@(�@�
@dZ@dZ@S�@33@o@��@�\@��@��@X@7L@7L@�@%@��@�@r�@A�@�@\)@;d@�@
=@
=@�R@$�@�T@��@/@�@��@��@��@�@��@�@z�@9X@��@�m@��@@
�\@
M�@
�@	��@	�#@	��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B;dB;dB;dB;dB<jB<jB<jB=qB=qB=qBA�BD�BC�BE�BG�BJ�BS�BW
BZB\)B]/BbNBdZBcTBcTB^5B]/B^5B_;B]/B^5BdZBffBjBt�B�B�%B�Bv�Bo�Bk�BiyBiyBdZB^5BXBE�B9XB7LB8RBI�BJ�BR�BZBO�BN�BM�BL�BK�BH�BE�BC�B49B#�B!�B�B�BuBoB�BbB�B.B33B&�B�BoB	7B��B�B��BȴB�FB��B��B�JB�BaHBA�B(�B�BoBB
��B
�`B
�B
ŢB
��B
�uB
_;B
M�B
?}B
49B
!�B
oB	��B	�TB	�#B	��B	��B	�}B	��B	�\B	�DB	�+B	u�B	n�B	hsB	]/B	R�B	K�B	C�B	5?B	$�B	�B	{B	%B��B�B�TB�B��BƨB�RB�B��B��B��B��B��B�oB�PB�B~�B}�B~�B�B�B� B}�Bz�Bw�Bu�Bu�Bq�Bp�Bo�Bl�BhsBe`BdZBbNBaHB`BB_;B^5B^5B\)BYBZBYBYBW
BW
BR�BQ�BP�BP�BO�BP�BM�BL�BJ�BI�BJ�BI�BE�BC�BA�B@�B?}B<jB<jB7LB6FB6FB6FB@�BI�BK�BG�BA�BI�BI�BE�B;dB7LB5?B49B49B0!B33B,B)�B/B-B+B+B/B0!B2-B6FB6FB8RB<jBB�BF�BI�BH�BI�BG�BD�B@�B=qB<jB<jBC�B@�B<jB<jBC�BC�BE�BI�BL�BM�BJ�BO�BS�BS�BS�BS�BVBXBZB]/BaHBcTBcTBcTBcTBiyBhsBiyBjBo�Bp�Br�Bt�Bv�Bv�Bw�Bv�Bx�Bz�B|�B�B�%B�+B�=B�DB�bB�hB�uB�oB��B��B��B��B��B��B��B��B��B��B�B�B�!B�'B�RB�jBBŢBȴB��B��B��B��B�B�B�5B�BB�NB�mB�mB�sB�yB�B�B�B��B��B��B��B��B��B��B	  B	B	%B	+B	1B	
=B	PB	uB	{B	�B	�B	�B	 �B	#�B	%�B	(�B	.B	1'B	1'B	33B	6FB	:^B	;dB	=qB	=qB	=qB	>wB	?}B	@�B	A�B	A�B	F�B	H�B	I�B	J�B	K�B	P�B	R�B	W
B	\)B	`BB	ffB	iyB	jB	k�B	n�B	p�B	r�B	t�B	v�B	w�B	|�B	�B	�B	�%B	�1B	�7B	�DB	�PB	�bB	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�!B	�'B	�'B	�'B	�'B	�'B	�-B	�-B	�-B	�3B	�?B	�FB	�LB	�XB	�^B	�dB	�dB	�dB	�dB	�jB	�qB	�qB	�wB	�}B	��B	��B	B	ÖB	ĜB	ŢB	ƨB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�#B	�)B	�#B	�)B	�/B	�/B	�5B	�5B	�;B	�BB	�BB	�HB	�NB	�TB	�TB	�TB	�ZB	�ZB	�fB	�mB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
%B
%B
+B
1B
1B
1B
1B
1B
1B

=B

=B
DB
JB
PB
PB
PB
VB
\B
\B
bB
bB
oB
oB
oB
oB
oB
uB
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
!�B
!�B
"�B
"�B
"�B
#�B
#�B
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
&�B
'�B
'�B
'�B
'�B
'�B
(�B
(�B
(�B
)�B
)�B
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
,B
,B
,B
,B
,B
-B
.B
.B
.B
.B
.B
.B
.B
0!B
0!B
0!B
0!B
1'B
1'B
2-B
2-B
2-B
2-B
2-B
33B
33B
33B
33B
33B
33B
49B
49B
49B
49B
5?B
5?B
5?B
5?B
5?B
6FB
6FB
6FB
7LB
7LB
7LB
7LB
8RB
8RB
8RB
9XB
9XB
9XB
9XB
9XB
:^B
:^B
:^B
:^B
:^B
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
?}B
?}B
?}B
@�B
@�B
@�B
@�B
@�B
@�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
B�B
B�B
B�B
B�B
B�B
B�B
B�B
C�B
C�B
C�B
C�B
D�B
D�B
D�B
D�B
D�B
E�B
E�B
E�B
F�B
E�B
F�B
E�B
F�B
F�B
G�B
G�B
G�B
H�B
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
I�B
J�B
J�B
J�B
J�B
J�B
J�B
K�B
K�B
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
M�B
M�B
M�B
N�B
N�B
N�B
N�B
N�B
O�B
N�B
O�B
O�B
O�B
O�B
P�B
P�B
Q�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
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
YB
YB
YB
YB
YB
YB
YB
YB
YB
ZB
ZB
ZB
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
^5B
_;B
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
`BB
`BB
`BB
`BB
`BB
`BB
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
bNB
bNB
bNB
bNB
cTB
bNB
bNB
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
gmB
gmB
gmB
gmB
hsB
hsB
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
jB
jB
k�B
k�B
l�B
l�B
l�B
l�B
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
m�B
m�B
n�B
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
q�B
r�B
r�B
r�B
r�B
r�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B;0B;0B;0B;0B<PB<6B<PB=<B=<B=VBAUBD�BCaBESBGEBJXBS�BV�BY�B\B]/Bb4Bd@BcTBcnB^B]B^B_VB]/B^Bd@Bf�Bj�ButB�AB�1B��By�Br�BmBl=Bm�Bf�BabB]�BJrB<�B:�B;BK�BNpBW�B\�BQhBO�BN�BN"BL�BI�BHBGEB6�B%,B#nB�B�BaB�BB�B#B/iB4�B'�B=B[B
�B��B�iB�hB�xB��B�ZB�xB�VB��Be`BDgB*B)BaB?B
��B
�RB
�B
ɆB
��B
��B
a�B
O\B
A�B
6�B
$�B
�B	�BB	��B	�)B	�&B	�pB	ðB	��B	�B	��B	�lB	v�B	p!B	jeB	^�B	T{B	M�B	FYB	7�B	&B	�B	�B	�B��B�9B�FB�]B��B��B��B��B�B��B�B�bB��B�FB��B��B�B~�B~�B�;B��B� B.B{BxlBv�Bv�BrGBrBqABm�Bi�Bf�Bd�Bb�Ba�B`�B_�B_;B_;B\�BZ�B[=BZQBZ7BXEBX+BTBS&BRBQ�BQNBQ�BN�BM�BKxBJ�BLdBJ�BF?BD�BB�BB�B@iB=VB=�B8RB6�B6�B6`BA�BJ�BM�BHfBA�BJrBK)BG_B<B8B5�B4�B5?B1'B4nB,�B*�B/�B-�B+�B+�B/iB0�B2�B6�B72B8�B<jBB'BF�BI�BI7BJ�BH�BE�BAUB=�B<�B<�BD�BAUB=VB>(BC�BC�BFBJ#BM�BN�BKBP.BTFBTBTaBTBVBX_BZ�B^Ba�Bc�BcnBc�BdtBi�Bh�Bi�BkBp!Bq�Bs3BuBv�BwBxBwfBy�B{0B}<B�;B�YB�EB�rB��B�}B��B��B��B��B��B�	B�B��B��B�bB�LB��B�0B�B�cB�UB��B��B��B�B�B��B��B��B�bB�@B�SB�B�OB�vB�B�B�RB�B�B�B��B��B��B��B��B�	B��B��B��B��B	�B	YB	�B	B	
	B	PB	�B	aB	gB	eB	�B	 �B	#�B	%�B	(�B	-�B	0�B	1AB	3MB	6`B	:^B	;0B	=<B	=<B	=VB	>wB	?�B	@�B	A�B	A�B	F�B	H�B	I�B	J�B	K�B	P�B	SB	W$B	\)B	`\B	fLB	iDB	jB	k�B	n}B	p�B	r�B	t�B	v�B	w�B	}"B	�B	��B	�%B	�1B	�7B	�)B	�PB	�HB	�NB	�[B	��B	�sB	�_B	�_B	�yB	�eB	�eB	��B	�qB	�qB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	� B	��B	��B	��B	��B	��B	�B	�B	��B	�B	�B	�B	�%B	�FB	�LB	�$B	�DB	�B	�0B	�0B	�0B	�PB	�VB	�<B	�]B	�cB	�oB	�oB	�uB	�aB	āB	ňB	ƨB	ɆB	ʦB	��B	��B	бB	��B	��B	��B	��B	�B	�B	��B	��B	��B	�B	�	B	�B	�#B	�B	�/B	�/B	�B	�B	�!B	�'B	�'B	�HB	�NB	�:B	�:B	�:B	�@B	�tB	�LB	�8B	�RB	�sB	�yB	�kB	�B	�B	�wB	�}B	�B	�B	�B	�B	�B	�|B	�|B	�B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B
B
�B
B
B
SB
%B
B
+B
B
B
�B
�B
B
1B

=B

#B
)B
0B
6B
6B
PB
VB
BB
BB
bB
HB
:B
:B
:B
TB
oB
uB
aB
{B
gB
gB
mB
YB
sB
yB
yB
�B
yB
B
�B
kB
kB
QB
WB
]B
xB
]B
�B
~B
~B
�B
�B
�B
�B
pB
 �B
 �B
!�B
!�B
!|B
!�B
"�B
"�B
"�B
#�B
#�B
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
&�B
'�B
'�B
'�B
'�B
'�B
(�B
(�B
(�B
)�B
)�B
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
+�B
+�B
+�B
,B
+�B
-)B
-�B
-�B
-�B
-�B
-�B
-�B
.B
/�B
0B
/�B
0B
1'B
1B
2B
2B
1�B
2-B
1�B
2�B
2�B
2�B
2�B
2�B
3B
4B
4B
4B
4B
5B
5B
5%B
5%B
5%B
6B
6+B
6B
72B
7B
72B
7LB
8B
8B
8B
9$B
9$B
9>B
9$B
9>B
:*B
:*B
:*B
:*B
:^B
;JB
;JB
<6B
<6B
<6B
<PB
<PB
<6B
=<B
=VB
=<B
=VB
>BB
>BB
>BB
?HB
?HB
?HB
@4B
@OB
@iB
@iB
@iB
@OB
AUB
AUB
AUB
A;B
A;B
AoB
AUB
B[B
B[B
B[B
BuB
B[B
BuB
BuB
C�B
C{B
CaB
C{B
D�B
DgB
DgB
D�B
DgB
E�B
E�B
EmB
FYB
EmB
FtB
E�B
FtB
F�B
GzB
GzB
G�B
H�B
H�B
H�B
H�B
H�B
H�B
IlB
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
K�B
K�B
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
M�B
M�B
M�B
N�B
N�B
N�B
N�B
N�B
O�B
N�B
O�B
O�B
O�B
O�B
P�B
P�B
Q�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
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
X�B
X�B
X�B
X�B
X�B
X�B
X�B
X�B
YB
Y�B
Y�B
Y�B
X�B
YB
X�B
ZB
ZB
ZB
ZB
[#B
[#B
Z�B
Z�B
[#B
[�B
[�B
[�B
[�B
\�B
\�B
\�B
]B
\�B
]B
^B
^�B
_B
^�B
_B
_B
_!B
_�B
_�B
`B
_�B
`B
`B
`B
`'B
`B
`B
`B
aB
aHB
aB
bB
bB
a�B
b4B
a�B
a�B
bB
a�B
bB
a�B
a�B
bB
cB
bB
b4B
c:B
c:B
c B
c B
d@B
d@B
d&B
d&B
eFB
eFB
e,B
eFB
f2B
f2B
fLB
f2B
f2B
f2B
g8B
g8B
g8B
g8B
hsB
hsB
iDB
iDB
iDB
i_B
iDB
jKB
j0B
j0B
jKB
jKB
jeB
jKB
jB
kkB
kkB
l=B
lWB
lqB
lWB
lqB
lqB
lWB
lqB
lqB
m]B
mwB
m]B
mCB
m]B
mwB
mwB
n}B
ncB
ncB
oOB
oiB
oOB
oOB
oOB
o�B
oiB
oiB
oiB
poB
p�B
p�B
p�B
qvB
qvB
r�B
r�B
r|B
r|B
ra111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=-0.53(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         202012220034012020122200340120201222003401202306231726352023062317263520230623172635202012230024572020122300245720201223002457  JA  ARFMdecpA19c                                                                20201217123943  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20201217034007  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20201217034008  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20201217034008  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20201217034009  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20201217034009  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20201217034009  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20201217034009  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20201217034009  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20201217034009                      G�O�G�O�G�O�                JA  ARUP                                                                        20201217035203                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20201217153530  CV  JULD            G�O�G�O�F�{�                JM  ARGQJMQC2.0                                                                 20201217153530  CV  JULD_LOCATION   G�O�G�O�F�{�                JM  ARGQJMQC2.0                                                                 20201217153530  CV  LONGITUDE       G�O�G�O��	�                JM  ARCAJMQC2.0                                                                 20201221153401  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20201221153401  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 CTD2019V1                                                       20201222152457  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20230623082635  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20230705041504                      G�O�G�O�G�O�                