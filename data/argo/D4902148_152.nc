CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       ~2018-10-15T18:36:00Z creation;2018-10-15T18:36:04Z conversion to V3.1;2019-12-18T07:19:34Z update;2022-11-21T05:30:00Z update;     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         BPRIMARY|https://orcid.org/0000-0001-9150-6442|Kanako Sato, JAMSTEC        @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    7H   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    7X   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    7\   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    7`   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7p   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7�   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  8   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        8H   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    8L   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    8P   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     8T   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8t   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8x   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8|   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
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
_FillValue                    9   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    9   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    9   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        :   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  :    PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ]$   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  a   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t|   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �d   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �h   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ݈   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20181015183600  20221123111507  4902148 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I1_0397_152                     2C  Dd�NAVIS_A                         0397                            ARGO 011514                     863 @؉#/4�1   @؉$��>�@<���-��d���o1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B ffB  B  BffB   B(  B/��B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZy�D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�9�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@�Q�A (�A (�A@(�A`(�A�{A�{A�{A�{A�{A�{A�{A�{B p�B
=B
=Bp�B 
=B(
=B/��B8
=B@
=BH
=BP
=BX
=B`
=Bh
=Bp
=Bx
=B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�C �C�C�C�C�C
�C�C�C�C�C�C�C�C�C�C�C �C"�C$�C&�C(�C*�C,�C.�C0�C2�C4�C6�C8�C:�C<�C>�C@�CB�CD�CF�CH�CJ�CL�CN�CP�CR�CT�CV�CX�CZ�C\�C^�C`�Cb�Cd�Cf�Ch�Cj�Cl�Cn�Cp�Cr�Ct�Cv�Cx�Cz�C|�C~�C�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC��{C�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC��{C�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HD  �D ��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D	 �D	��D
 �D
��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D  �D ��D! �D!��D" �D"��D# �D#��D$ �D$��D% �D%��D& �D&��D' �D'��D( �D(��D) �D)��D* �D*��D+ �D+��D, �D,��D- �D-��D. �D.��D/ �D/��D0 �D0��D1 �D1��D2 �D2��D3 �D3��D4 �D4��D5 �D5��D6 �D6��D7 �D7��D8 �D8��D9 �D9��D: �D:��D; �D;��D< �D<��D= �D=��D> �D>��D? �D?��D@ �D@��DA �DA��DB �DB��DC �DC��DD �DD��DE �DE��DF �DF��DG �DG��DH �DH��DI �DI��DJ �DJ��DK �DK��DL �DL��DM �DM��DN �DN��DO �DO��DP �DP��DQ �DQ��DR �DR��DS �DS��DT �DT��DU �DU��DV �DV��DW �DW��DX �DX��DY �DY��DZ �DZz=D[ �D[��D\ �D\��D] �D]��D^ �D^��D_ �D_��D` �D`��Da �Da��Db �Db��Dc �Dc��Dd �Dd��De �De��Df �Df��Dg �Dg��Dh �Dh��Di �Di��Dj �Dj��Dk �Dk��Dl �Dl��Dm �Dm��Dn �Dn��Do �Do��Dp �Dp��Dq �Dq��Dr �Dr��Ds �Ds��Dt �Dt��Du �Du��Dv �Dv��Dw �Dw��Dx �Dx��Dy �Dy��Dz �Dz��D{ �D{��D| �D|��D} �D}��D~ �D~��D �D��D� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�=D��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RDRD��RD� RD�@RDÀRD��RD� RD�@RDĀRD��RD� RD�@RDŀRD��RD� RD�@RDƀRD��RD� RD�@RDǀRD��RD� RD�@RDȀRD��RD� RD�@RDɀRD��RD� RD�@RDʀRD��RD� RD�@RDˀRD��RD� RD�@RD̀RD��RD� RD�@RD̀RD��RD� RD�@RD΀RD��RD� RD�@RDπRD��RD� RD�@RDЀRD��RD� RD�@RDрRD��RD� RD�@RDҀRD��RD� RD�@RDӀRD��RD� RD�@RDԀRD��RD� RD�@RDՀRD��RD� RD�@RDրRD��RD� RD�@RD׀RD��RD� RD�@RD؀RD��RD� RD�@RDـRD��RD� RD�@RDڀRD��RD� RD�@RDۀRD��RD� RD�@RD܀RD��RD� RD�@RD݀RD��RD� RD�@RDހRD��RD� RD�@RD߀RD��RD� RD�@RD��RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD��RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD��D�9�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�^5A�^5A�\)A�\)A�ZA�\)A�^5A�bNA�jA�hsA�ffA�hsA�ffA�ffA�bNA�dZA�dZA�dZA�^5A���A�;dA��HA��DA�=qA�I�A��A�"�A���A���A��7A�z�A��A���A��A��A��TA�7LA�1A���A��A�%A�oA��FA�1'A�M�A�{A��A��DA�1'A���A�C�A��TA� �A��A���A���A�bNA�jA�|�A��TA��A�K�A�+A���A���A���A�p�A���A�1A��PA�Q�A��A�JA��A��A�|�A�z�A��A��A�FA|�A~�A}��A|��A{�wAz��AzM�Ay��AvZAr��Aq�
ApȴAo�#Ao�An��Am|�Al�Ak`BAj�Aj�!Ajv�Aj$�Ai�AhJAeƨAcC�AaO�A_7LA^�A^{A]VA\ffA\-A\  A[��A[\)AZ�yAZE�AY`BAX��AX�AX�AWG�AVE�AUp�AT��ATbNAT9XAS�hAR��AQ|�ANĜAMhsALM�AKC�AJ�!AI�PAH��AH(�AF�AE��AD��AC�wAB��AA�FAA�A@��A@��A@ffA@{A?�
A>�RA>�A=��A=��A<�yA<�DA<^5A<9XA;��A;��A;C�A:��A:r�A:1A9?}A8~�A7�A6jA5�A5x�A5K�A5�A4�A2�RA1S�A0ĜA/��A/C�A.��A.Q�A-&�A+��A*��A*�RA*Q�A)�wA)|�A)G�A(�A(�DA(5?A&��A&  A%�A%�#A%��A$jA#t�A"$�A �A �uA ZA {A�mA�
A�-AO�Az�AhsA-A�AĜA�AoA�RA��AG�A��A9XA�AoA��A��AbNA��AhsAS�A�A�TAn�AA��A�A\)A�uA1'Al�A�A��A�A
�uA	p�A�HA$�A�A��A�AA  A��A&�A Z@�33@�@��u@�K�@���@��@�%@�(�@�dZ@�5?@�1'@� �@�@�j@�!@��@�  @�S�@�+@��/@�z�@�1'@��;@��@�5?@݉7@�V@���@�Z@� �@�ƨ@���@ۥ�@�K�@�"�@��@���@ڟ�@�=q@ٺ^@�&�@أ�@�j@�I�@�z�@�bN@�Q�@�+@ְ!@֗�@�5?@�@ղ-@Ձ@�V@ԛ�@ӍP@�p�@��@�n�@�dZ@��h@��D@�b@���@�"�@��+@��@�O�@��@��;@��@�J@���@��7@���@�j@��
@�S�@��@�$�@���@���@� �@��;@��P@�o@���@��@��/@��;@�^5@�V@�bN@�A�@���@�C�@�o@���@�=q@���@���@�j@�b@�|�@���@��@�X@��/@��D@��@�dZ@�33@��@�@��H@�ȴ@��R@�~�@�E�@���@���@�E�@��@��T@��^@��h@��@�hs@�?}@��`@��9@���@�r�@�A�@��F@�;d@�
=@�ȴ@�X@��;@�C�@��y@�5?@�7L@�/@�&�@�V@�9X@��
@�ƨ@��@��@�;d@�33@�o@���@�~�@�^5@�5?@���@�hs@�?}@�/@���@��/@��@� �@���@�\)@��R@��@��@��-@�X@���@��D@�A�@���@�C�@���@���@�M�@�E�@�E�@�5?@�-@��@��@�7L@�%@���@���@���@�r�@�Q�@�9X@�(�@���@��F@�;d@��+@�{@��^@��h@�x�@�X@�7L@�Ĝ@�r�@�1'@�;@�w@��@�P@|�@�@~�@~@}p�@|��@|I�@|1@{dZ@{o@z�@z��@zJ@yX@y�@x��@xQ�@xbN@xA�@x �@w��@w;d@vȴ@vV@u@up�@uV@tz�@sƨ@st�@r��@q�^@p�9@pr�@pQ�@p1'@o�@o��@o��@o�P@o�@o�@ol�@oK�@o;d@o+@nv�@nV@m�T@m@m��@m�T@m�T@m?}@l��@l�D@lz�@k��@k�@kdZ@kS�@kS�@k"�@j�H@j��@j~�@jn�@jM�@j=q@j�@i��@i�7@hA�@g�;@g�w@f��@e�h@eV@dz�@d�@cdZ@co@c@c@b��@b��@b~�@b-@bJ@a�^@a�7@ax�@`��@`  @_\)@^v�@]��@]�h@]O�@\�j@\z�@\j@\Z@[��@[��@[S�@["�@[o@Z��@Zn�@Zn�@ZM�@Y��@Y7L@X�u@X�@XA�@W��@Wl�@Wl�@Wl�@WK�@V��@Vȴ@V�R@V�+@VE�@U�-@UV@T�/@T�/@T9X@T�@S�m@S�@SS�@So@So@R��@R�!@R��@R�\@R=q@Q�@P��@P��@P�@Pr�@PbN@PA�@P  @O�w@O�P@O|�@OK�@O�@O
=@N��@N�y@N�y@Nȴ@Nȴ@N��@NE�@N5?@N{@M?}@L�@Lj@L(�@K�
@K�@KdZ@K"�@J�@J�\@J=q@H��@HbN@HQ�@HA�@G�;@G\)@Fȴ@F��@F��@F�+@Fff@FE�@F{@F@E�@E�T@E�T@E�@E�@E�T@E��@E��@E@E��@EO�@D��@D�@D�D@D1@Ct�@C33@B��@B~�@B=q@B�@BJ@A�#@A��@AG�@@�`@@�9@@�@@1'@?��@?
=@>�@>��@>5?@=��@=�@=?}@=V@<�/@<�D@<z�@<9X@;�m@;��@;��@;�@;��@;�@;t�@;33@;33@;o@;@:��@:�!@:n�@9�@9��@9��@9��@9x�@97L@8��@8�`@8��@8Ĝ@8�u@8Q�@7��@7+@6ȴ@6v�@6E�@6@5��@5/@4��@4�j@4��@4�D@4z�@4z�@4Z@49X@3��@3�
@3��@3�@3C�@2^5@1��@1hs@0��@/�;@/�w@/�P@/\)@/K�@/K�@/;d@/;d@/+@/+@.��@.�R@.��@.��@.E�@.@-p�@,j@,�@+�
@+ƨ@+�@+"�@*�!@*~�@*=q@*J@)�#@)�7@)hs@)�@(�u@(Q�@(  @'��@'|�@'�@&��@&�R@&V@&E�@&$�@&$�@&@%�-@%V@$��@$�D@$z�@#��@#�F@#��@#�@#dZ@#S�@#33@"�!@"-@!�#@!hs@!7L@!&�@!�@!%@ �9@�@�y@ȴ@�R@�+@v�@ff@E�@E�@5?@$�@@@?}@��@��@��@�D@Z@�
@dZ@@�H@�!@~�@n�@n�@n�@n�@n�@n�@^5@M�@M�@M�@=q@-@-@J@�@�#@��@��@hs@7L@�`@�9@1'@�@�;@��@�@��@\)@�@�y@ȴ@�R@��@�+@ff@{@��@�@?}@��@�/@��@�j@z�@�@ƨ@�@dZ@C�@33@o@�@�H@�H@�!@~�@M�@�#@�^@�^@�^@�^@��@X@�@�@%@�@��@�u@ �@  @�@K�@+@��@�@��@ff@5?@��@/@�@�@I�@(�@1@�
@�F@��@�@�@S�@
n�@
=q@
=q@
-@
J@
J@	�@	��@	��@	��@	�7@	�7@	x�@	X@	7L@	&�@��@Ĝ@�9@��@�u@r�@bN@bN@ �@�;@�P@\)@;d@+@
=@
=@
=@
=@�y@ȴ@�+@$�@�T@�-@O�@/@��@�@�@�/@�j@�D@Z@9X@9X@(�@(�@�@�m@�
@�F@��@��@t�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�^5A�^5A�\)A�\)A�ZA�\)A�^5A�bNA�jA�hsA�ffA�hsA�ffA�ffA�bNA�dZA�dZA�dZA�^5A���A�;dA��HA��DA�=qA�I�A��A�"�A���A���A��7A�z�A��A���A��A��A��TA�7LA�1A���A��A�%A�oA��FA�1'A�M�A�{A��A��DA�1'A���A�C�A��TA� �A��A���A���A�bNA�jA�|�A��TA��A�K�A�+A���A���A���A�p�A���A�1A��PA�Q�A��A�JA��A��A�|�A�z�A��A��A�FA|�A~�A}��A|��A{�wAz��AzM�Ay��AvZAr��Aq�
ApȴAo�#Ao�An��Am|�Al�Ak`BAj�Aj�!Ajv�Aj$�Ai�AhJAeƨAcC�AaO�A_7LA^�A^{A]VA\ffA\-A\  A[��A[\)AZ�yAZE�AY`BAX��AX�AX�AWG�AVE�AUp�AT��ATbNAT9XAS�hAR��AQ|�ANĜAMhsALM�AKC�AJ�!AI�PAH��AH(�AF�AE��AD��AC�wAB��AA�FAA�A@��A@��A@ffA@{A?�
A>�RA>�A=��A=��A<�yA<�DA<^5A<9XA;��A;��A;C�A:��A:r�A:1A9?}A8~�A7�A6jA5�A5x�A5K�A5�A4�A2�RA1S�A0ĜA/��A/C�A.��A.Q�A-&�A+��A*��A*�RA*Q�A)�wA)|�A)G�A(�A(�DA(5?A&��A&  A%�A%�#A%��A$jA#t�A"$�A �A �uA ZA {A�mA�
A�-AO�Az�AhsA-A�AĜA�AoA�RA��AG�A��A9XA�AoA��A��AbNA��AhsAS�A�A�TAn�AA��A�A\)A�uA1'Al�A�A��A�A
�uA	p�A�HA$�A�A��A�AA  A��A&�A Z@�33@�@��u@�K�@���@��@�%@�(�@�dZ@�5?@�1'@� �@�@�j@�!@��@�  @�S�@�+@��/@�z�@�1'@��;@��@�5?@݉7@�V@���@�Z@� �@�ƨ@���@ۥ�@�K�@�"�@��@���@ڟ�@�=q@ٺ^@�&�@أ�@�j@�I�@�z�@�bN@�Q�@�+@ְ!@֗�@�5?@�@ղ-@Ձ@�V@ԛ�@ӍP@�p�@��@�n�@�dZ@��h@��D@�b@���@�"�@��+@��@�O�@��@��;@��@�J@���@��7@���@�j@��
@�S�@��@�$�@���@���@� �@��;@��P@�o@���@��@��/@��;@�^5@�V@�bN@�A�@���@�C�@�o@���@�=q@���@���@�j@�b@�|�@���@��@�X@��/@��D@��@�dZ@�33@��@�@��H@�ȴ@��R@�~�@�E�@���@���@�E�@��@��T@��^@��h@��@�hs@�?}@��`@��9@���@�r�@�A�@��F@�;d@�
=@�ȴ@�X@��;@�C�@��y@�5?@�7L@�/@�&�@�V@�9X@��
@�ƨ@��@��@�;d@�33@�o@���@�~�@�^5@�5?@���@�hs@�?}@�/@���@��/@��@� �@���@�\)@��R@��@��@��-@�X@���@��D@�A�@���@�C�@���@���@�M�@�E�@�E�@�5?@�-@��@��@�7L@�%@���@���@���@�r�@�Q�@�9X@�(�@���@��F@�;d@��+@�{@��^@��h@�x�@�X@�7L@�Ĝ@�r�@�1'@�;@�w@��@�P@|�@�@~�@~@}p�@|��@|I�@|1@{dZ@{o@z�@z��@zJ@yX@y�@x��@xQ�@xbN@xA�@x �@w��@w;d@vȴ@vV@u@up�@uV@tz�@sƨ@st�@r��@q�^@p�9@pr�@pQ�@p1'@o�@o��@o��@o�P@o�@o�@ol�@oK�@o;d@o+@nv�@nV@m�T@m@m��@m�T@m�T@m?}@l��@l�D@lz�@k��@k�@kdZ@kS�@kS�@k"�@j�H@j��@j~�@jn�@jM�@j=q@j�@i��@i�7@hA�@g�;@g�w@f��@e�h@eV@dz�@d�@cdZ@co@c@c@b��@b��@b~�@b-@bJ@a�^@a�7@ax�@`��@`  @_\)@^v�@]��@]�h@]O�@\�j@\z�@\j@\Z@[��@[��@[S�@["�@[o@Z��@Zn�@Zn�@ZM�@Y��@Y7L@X�u@X�@XA�@W��@Wl�@Wl�@Wl�@WK�@V��@Vȴ@V�R@V�+@VE�@U�-@UV@T�/@T�/@T9X@T�@S�m@S�@SS�@So@So@R��@R�!@R��@R�\@R=q@Q�@P��@P��@P�@Pr�@PbN@PA�@P  @O�w@O�P@O|�@OK�@O�@O
=@N��@N�y@N�y@Nȴ@Nȴ@N��@NE�@N5?@N{@M?}@L�@Lj@L(�@K�
@K�@KdZ@K"�@J�@J�\@J=q@H��@HbN@HQ�@HA�@G�;@G\)@Fȴ@F��@F��@F�+@Fff@FE�@F{@F@E�@E�T@E�T@E�@E�@E�T@E��@E��@E@E��@EO�@D��@D�@D�D@D1@Ct�@C33@B��@B~�@B=q@B�@BJ@A�#@A��@AG�@@�`@@�9@@�@@1'@?��@?
=@>�@>��@>5?@=��@=�@=?}@=V@<�/@<�D@<z�@<9X@;�m@;��@;��@;�@;��@;�@;t�@;33@;33@;o@;@:��@:�!@:n�@9�@9��@9��@9��@9x�@97L@8��@8�`@8��@8Ĝ@8�u@8Q�@7��@7+@6ȴ@6v�@6E�@6@5��@5/@4��@4�j@4��@4�D@4z�@4z�@4Z@49X@3��@3�
@3��@3�@3C�@2^5@1��@1hs@0��@/�;@/�w@/�P@/\)@/K�@/K�@/;d@/;d@/+@/+@.��@.�R@.��@.��@.E�@.@-p�@,j@,�@+�
@+ƨ@+�@+"�@*�!@*~�@*=q@*J@)�#@)�7@)hs@)�@(�u@(Q�@(  @'��@'|�@'�@&��@&�R@&V@&E�@&$�@&$�@&@%�-@%V@$��@$�D@$z�@#��@#�F@#��@#�@#dZ@#S�@#33@"�!@"-@!�#@!hs@!7L@!&�@!�@!%@ �9@�@�y@ȴ@�R@�+@v�@ff@E�@E�@5?@$�@@@?}@��@��@��@�D@Z@�
@dZ@@�H@�!@~�@n�@n�@n�@n�@n�@n�@^5@M�@M�@M�@=q@-@-@J@�@�#@��@��@hs@7L@�`@�9@1'@�@�;@��@�@��@\)@�@�y@ȴ@�R@��@�+@ff@{@��@�@?}@��@�/@��@�j@z�@�@ƨ@�@dZ@C�@33@o@�@�H@�H@�!@~�@M�@�#@�^@�^@�^@�^@��@X@�@�@%@�@��@�u@ �@  @�@K�@+@��@�@��@ff@5?@��@/@�@�@I�@(�@1@�
@�F@��@�@�@S�@
n�@
=q@
=q@
-@
J@
J@	�@	��@	��@	��@	�7@	�7@	x�@	X@	7L@	&�@��@Ĝ@�9@��@�u@r�@bN@bN@ �@�;@�P@\)@;d@+@
=@
=@
=@
=@�y@ȴ@�+@$�@�T@�-@O�@/@��@�@�@�/@�j@�D@Z@9X@9X@(�@(�@�@�m@�
@�F@��@��@t�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�bB�\B�bB�bB�bB�bB�bB�bB�hB�uB��B�B�B��B�7B�oB��B�VB�VB��B��B�uB�7B�B�B{�Bu�Bp�BgmB_;B>wB+B{BhB\BbB�B{B��B�B��B��B��B��B�'B��B��B�+By�Bt�Bn�Be`BcTB\)BH�B<jB7LB49B'�B �B�BPB
�B
�;B
�}B
��B
�+B
}�B
t�B
hsB
gmB
dZB
^5B
XB
R�B
M�B
H�B
B�B
2-B
�B
�B
uB
VB
DB
+B	��B	��B	��B	�B	�B	�B	�B	�yB	�BB	��B	ȴB	�qB	�3B	�?B	�3B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�uB	�\B	�7B	�B	�B	� B	~�B	z�B	v�B	o�B	aHB	ZB	VB	R�B	O�B	J�B	E�B	B�B	<jB	7LB	33B	.B	)�B	$�B	#�B	 �B	�B	�B	�B	�B	�B	oB	bB	\B	PB	DB	
=B		7B	1B	+B	B	B	B��B��B��B�B�B�B�yB�mB�`B�HB�B��B��B��B��B��B��BƨB��B�wB�jB�dB�XB�RB�LB�?B�3B�'B�B�B��B��B��B��B��B��B��B��B��B��B��B�{B�uB�hB�VB�DB�+B�B�B�B}�B{�By�Bx�Bv�Bu�Bs�Br�Bq�Bp�Bo�Bn�Bm�Bl�BjBgmBdZBcTBcTBbNBaHB_;B]/B[#BYBXBVBR�BP�BN�BM�BK�BJ�BI�BH�BG�BD�B?}B;dB9XB8RB7LB6FB5?B49B49B2-B1'B/B+B)�B(�B'�B'�B&�B&�B%�B%�B%�B%�B$�B$�B$�B$�B$�B$�B$�B#�B#�B$�B#�B#�B#�B#�B#�B#�B#�B#�B#�B#�B#�B#�B#�B#�B#�B"�B#�B#�B#�B#�B#�B#�B#�B#�B"�B!�B �B�B"�B-B1'B33B33B49B5?B6FB6FB7LB7LB9XB9XB<jB<jB<jB=qB>wB@�BA�BC�BE�BI�BI�BK�BK�BL�BM�BQ�BS�BVBYB]/B`BBbNBbNBbNBcTBcTBdZBe`BhsBl�Bn�Bn�Bo�Bq�Bs�Bu�Bx�By�B|�B� B�B�B�B�B�B�B�B�B�B�PB�oB�uB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�!B�9B�?B�RB�qB�wB�wB�wBĜBǮBȴB��B��B��B��B��B��B��B��B��B��B�
B�B�B�B�B�)B�;B�NB�ZB�sB�B�B�B�B��B��B��B	  B	B	B	%B	1B	1B	1B	1B	1B	
=B	VB	hB	oB	{B	�B	�B	�B	�B	�B	�B	�B	"�B	$�B	%�B	'�B	)�B	+B	,B	.B	/B	1'B	33B	5?B	9XB	9XB	:^B	;dB	;dB	=qB	>wB	B�B	D�B	F�B	H�B	I�B	L�B	M�B	N�B	O�B	P�B	S�B	T�B	XB	YB	[#B	]/B	_;B	`BB	`BB	bNB	ffB	ffB	gmB	iyB	k�B	m�B	n�B	o�B	q�B	t�B	u�B	v�B	v�B	w�B	y�B	z�B	}�B	~�B	� B	�B	�B	�B	�B	�%B	�+B	�7B	�DB	�DB	�JB	�PB	�VB	�\B	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�!B	�!B	�'B	�'B	�3B	�9B	�?B	�FB	�LB	�LB	�^B	�jB	�wB	��B	ÖB	ÖB	ŢB	ǮB	ȴB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�#B	�)B	�)B	�/B	�/B	�/B	�;B	�HB	�NB	�NB	�ZB	�ZB	�`B	�fB	�mB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
%B
+B
1B
1B
1B
	7B
	7B

=B

=B

=B

=B

=B

=B

=B

=B

=B

=B

=B

=B
DB
JB
JB
PB
PB
\B
\B
bB
bB
hB
hB
hB
hB
oB
oB
uB
uB
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
�B
 �B
!�B
!�B
!�B
!�B
!�B
!�B
!�B
!�B
!�B
!�B
"�B
#�B
#�B
$�B
$�B
&�B
'�B
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
,B
,B
,B
,B
.B
/B
/B
0!B
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
33B
33B
33B
33B
33B
33B
49B
5?B
5?B
6FB
6FB
6FB
7LB
8RB
8RB
8RB
8RB
8RB
9XB
9XB
:^B
:^B
;dB
;dB
;dB
<jB
=qB
=qB
=qB
>wB
>wB
>wB
>wB
>wB
>wB
@�B
@�B
A�B
A�B
B�B
C�B
C�B
C�B
C�B
C�B
D�B
D�B
E�B
F�B
F�B
G�B
G�B
G�B
G�B
G�B
I�B
J�B
J�B
J�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
L�B
M�B
M�B
M�B
M�B
N�B
N�B
O�B
P�B
P�B
P�B
P�B
Q�B
Q�B
P�B
Q�B
Q�B
Q�B
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
S�B
S�B
S�B
T�B
T�B
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
XB
XB
XB
XB
XB
YB
YB
YB
ZB
ZB
[#B
[#B
[#B
[#B
[#B
\)B
\)B
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
_;B
_;B
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
bNB
bNB
cTB
cTB
cTB
dZB
dZB
dZB
e`B
e`B
e`B
ffB
gmB
gmB
gmB
hsB
hsB
hsB
hsB
iyB
iyB
iyB
iyB
iyB
jB
k�B
k�B
k�B
k�B
k�B
k�B
k�B
l�B
k�B
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
m�B
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
o�B
p�B
p�B
q�B
q�B
q�B
r�B
r�B
r�B
r�B
r�B
r�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
t�B
t�B
t�B
t�B
t�B
t�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�bB�\B�bB�bB�bB�}B��B�NB�,B��B��B�LB�2B��B��B�SB��B�NB�vB�mB�B�EB��B�B��B}<BvzBrBi�Bb�BAUB/OBB&B.B�BEB?B�rB�B�aB�B�B�mB�hB��B��B�7B{dBv�Bp!BfBd�B^�BJXB=�B8�B6+B)*B!�B�B�B
�tB
�B
�MB
�B
�B
�B
v�B
iB
hXB
e�B
_pB
YKB
S�B
N�B
JrB
FYB
5�B
!-B
�B
{B
B
dB
�B
 �B	��B	�?B	�B	�B	�;B	��B	�B	�:B	��B	�)B	��B	�B	�+B	�TB	��B	�DB	�8B	�LB	�tB	��B	��B	��B	�QB	�B	�?B	��B	��B	�XB	�B	�uB	��B	� B	|jB	x�B	r�B	cB	[�B	WYB	S�B	QNB	K�B	F�B	D3B	=�B	8�B	4�B	/iB	+B	%FB	$tB	!-B	;B	/B	=B	�B	9B	�B	�B	.B	�B	�B	
�B		�B	�B	�B	�B	�B	�B	 B�B�$B��B�IB�"B��B�
B�B�nB��B��B� B�pB�jB��B�dB�1BB��B�B�B��B��B��B��B�B��B��B�6B�eB��B�yB�FB�bB��B�B��B��B��B��B��B�FB��B��B��B�1B�?B�3B�'B~�B}Bz�ByrBw�Bv�BtTBsBrBq'BpoBo Bm�BmCBl=BiBd�Bc�Bc�Bc:BbNB`B^OB\]BY�BX�BWYBTaBQ�BO�BN�BL�BK�BJ	BI7BIBG�BB�B<�B:DB9XB8B6�B5�B5B4�B2�B2|B1vB/iB+�B*B)*B)B'�B'�B&�B&�B&LB&2B%FB%�B%zB%`B%,B%,B%FB$&B$&B$�B$B$&B$B$&B$B$B$@B$ZB$ZB$@B$B#�B#�B$B$&B#�B$@B$B$&B$B#�B$&B$ZB$tB$B#�B#�B#:B&�B./B1�B3�B3�B4�B5�B6�B6�B7�B7�B9�B:DB<�B<�B<�B=�B>�BABBBD3BFtBJ	BJ#BLBL0BMPBN�BRoBT�BV�BZ7B^B`�BbhBb�Bb�Bc�Bc�Bd�Be�BiBl�Bo BoBpUBr-Bt9Bv+By$BzDB}VB�4B�B� B�AB�'B�AB�AB�{B��B�mB�"B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�-B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B��B�B�B�4B�FB�?B�+B�EB�KB�kBܒBߤB�B��B��B��B��B��B��B�B�*B�PB	 iB	aB	gB	YB	KB	KB	KB	KB	fB	
rB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	#:B	%FB	&2B	(>B	*B	+B	,=B	.IB	/OB	1vB	3hB	5tB	9XB	9rB	:xB	;B	;�B	=�B	>�B	B�B	D�B	F�B	H�B	J	B	L�B	M�B	N�B	PB	Q4B	T,B	U2B	XB	YB	[=B	]IB	_�B	`�B	`vB	b�B	f�B	f�B	g�B	i�B	k�B	m�B	n�B	pB	q�B	t�B	u�B	v�B	v�B	w�B	y�B	z�B	~B	B	�B	� B	�'B	�'B	�[B	�?B	�zB	�RB	�DB	�)B	�dB	��B	��B	�\B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	�,B	�ZB	�B	�8B	�0B	�QB	�/B	�!B	�;B	�;B	�AB	�AB	�MB	�TB	�ZB	�`B	�fB	��B	��B	��B	��B	��B	ðB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	� B	�B	�B	�2B	�$B	�B	�1B	�B	�=B	�CB	�CB	�IB	�dB	�dB	�pB	�bB	�hB	�B	�tB	�tB	�zB	�B	�B	�yB	�B	�B	�B	�B	�B	�B	��B	�B	�B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	�B	�B	�B	�B	�B	�B	�.B
 B
oB
GB
3B
3B
MB
SB
?B
EB
B
KB
KB
	RB
	RB

=B

=B

=B

=B

=B

XB

=B

XB

=B

=B

XB

rB
^B
dB
dB
�B
�B
vB
�B
}B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
�B
 �B
!�B
!�B
!�B
!�B
!�B
!�B
!�B
!�B
!�B
"B
#B
#�B
#�B
%B
$�B
&�B
($B
)B
)�B
)�B
*�B
+B
+B
+B
+B
+B
,"B
,"B
,"B
,=B
,WB
.IB
/OB
/iB
0oB
2GB
2GB
2GB
2-B
33B
33B
33B
33B
33B
3MB
3MB
33B
3MB
3hB
3hB
3�B
4�B
5ZB
5ZB
6`B
6`B
6�B
7�B
8lB
8lB
8lB
8lB
8lB
9rB
9�B
:�B
:xB
;B
;B
;B
<�B
=�B
=�B
=�B
>wB
>�B
>�B
>�B
>�B
>�B
@�B
@�B
A�B
A�B
B�B
C�B
C�B
C�B
C�B
C�B
D�B
D�B
E�B
F�B
F�B
G�B
G�B
G�B
G�B
HB
I�B
J�B
J�B
J�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
L�B
M�B
M�B
M�B
M�B
OB
OB
O�B
P�B
Q B
Q B
P�B
Q�B
Q�B
P�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
RB
RB
R�B
R�B
SB
TB
TB
TB
UB
U2B
VB
W
B
V�B
W$B
W$B
W$B
W$B
X+B
X+B
X+B
X+B
X+B
Y1B
Y1B
Y1B
Z7B
Z7B
[=B
[#B
[#B
[WB
[=B
\)B
\CB
]IB
]IB
]IB
^5B
^OB
^OB
^5B
^5B
^OB
^OB
_;B
_;B
`BB
`'B
`BB
`BB
`\B
`\B
`\B
aHB
a-B
aHB
abB
abB
bNB
b�B
cTB
cnB
cnB
dtB
dtB
dtB
ezB
ezB
e�B
f�B
g�B
g�B
gmB
h�B
h�B
h�B
h�B
i�B
i_B
i�B
i�B
i�B
j�B
kkB
kkB
k�B
k�B
k�B
k�B
k�B
l�B
k�B
l�B
l�B
l�B
l�B
l�B
l�B
mwB
m�B
m�B
m�B
m�B
m�B
m�B
m�B
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
o�B
p�B
p�B
q�B
q�B
q�B
r�B
r�B
r�B
r�B
r�B
r�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
t�B
t�B
t�B
t�B
t�B
t�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
</O<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=-0.01(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201810260038322018102600383220181026003832202211182136362022111821363620221118213636201810270021002018102700210020181027002100  JA  ARFMdecpA19c                                                                20181016033518  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20181015183600  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20181015183602  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20181015183602  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20181015183603  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20181015183603  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20181015183603  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20181015183603  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20181015183603  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20181015183604                      G�O�G�O�G�O�                JA  ARUP                                                                        20181015185538                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20181016153452  CV  JULD            G�O�G�O�F�I                JM  ARCAJMQC2.0                                                                 20181025153832  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20181025153832  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20181026152100  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200114171536                      G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20221118123636  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20221123111507                      G�O�G�O�G�O�                