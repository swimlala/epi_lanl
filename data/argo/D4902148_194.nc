CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2019-12-09T15:37:15Z creation;2019-12-09T15:37:20Z conversion to V3.1;2022-11-21T05:27:54Z update;     
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
_FillValue                 �  ]@   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  a,   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �h   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �T   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �|   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �$   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �t   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �    HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20191209153715  20221123111508  4902148 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I1_0397_194                     2C  D   NAVIS_A                         0397                            ARGO 011514                     863 @���8 1   @�� ��À@;���ڹ��dr^5?|�1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DLy�DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ D�|�D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D��3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@�Q�A (�A (�A@(�A`(�A�{A�{A�{A�{A�{A�{A�{A�{B 
=B
=B
=B
=B 
=B(
=B0
=B8
=B@
=BH
=BP
=BX
=B`
=Bh
=Bp
=Bx
=B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�C �C�C�C�C�C
�C�C�C�C�C�C�C�C�C�C�C �C"�C$�C&�C(�C*�C,�C.�C0�C2�C4�C6�C8�C:�C<�C>�C@�CB�CD�CF�CH�CJ�CL�CN�CP�CR�CT�CV�CX�CZ�C\�C^�C`�Cb�Cd�Cf�Ch�Cj�Cl�Cn�Cp�Cr�Ct�Cv�Cx�Cz�C|�C~�C�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HD  �D ��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D	 �D	��D
 �D
��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D  �D ��D! �D!��D" �D"��D# �D#��D$ �D$��D% �D%��D& �D&��D' �D'��D( �D(��D) �D)��D* �D*��D+ �D+��D, �D,��D- �D-��D. �D.��D/ �D/��D0 �D0��D1 �D1��D2 �D2��D3 �D3��D4 �D4��D5 �D5��D6 �D6��D7 �D7��D8 �D8��D9 �D9��D: �D:��D; �D;��D< �D<��D= �D=��D> �D>��D? �D?��D@ �D@��DA �DA��DB �DB��DC �DC��DD �DD��DE �DE��DF �DF��DG �DG��DH �DH��DI �DI��DJ �DJ��DK �DK��DL �DLz=DM �DM��DN �DN��DO �DO��DP �DP��DQ �DQ��DR �DR��DS �DS��DT �DT��DU �DU��DV �DV��DW �DW��DX �DX��DY �DY��DZ �DZ��D[ �D[��D\ �D\��D] �D]��D^ �D^��D_ �D_��D` �D`��Da �Da��Db �Db��Dc �Dc��Dd �Dd��De �De��Df �Df��Dg �Dg��Dh �Dh��Di �Di��Dj �Dj��Dk �Dk��Dl �Dl��Dm �Dm��Dn �Dn��Do �Do��Dp �Dp��Dq �Dq��Dr �Dr��Ds �Ds��Dt �Dt��Du �Du��Dv �Dv��Dw �Dw��Dx �Dx��Dy �Dy��Dz �Dz��D{ �D{��D| �D|��D} �D}��D~ �D~��D �D��D� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RDRD��RD� RD�@RDÀRD��RD� RD�@RDĀRD��RD� RD�@RDŀRD��RD� RD�@RDƀRD��RD� RD�@RDǀRD��RD� RD�@RDȀRD��RD� RD�@RDɀRD��RD� RD�@RDʀRD��RD� RD�@RDˀRD��RD� RD�@RD̀RD��RD� RD�@RD̀RD��RD� RD�@RD΀RD��RD� RD�@RDπRD��RD� RD�@RDЀRD��RD� RD�@RDрRD��RD� RD�@RDҀRD��RD� RD�@RDӀRD��RD� RD�@RDԀRD��RD� RD�@RD�}D��RD� RD�@RDրRD��RD� RD�@RD׀RD��RD� RD�@RD؀RD��RD� RD�@RDـRD��RD� RD�@RDڀRD��RD� RD�@RDۀRD��RD� RD�@RD܀RD��RD� RD�@RD݀RD��RD� RD�@RDހRD��RD� RD�@RD߀RD��RD� RD�@RD��RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD��RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�C�D��RD���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�S�A�ZA�VA�O�A�`BA�ffA�hsA�hsA�hsA�jA�hsA�jA�hsA�jA�l�A�n�A�p�A�p�A�t�A�t�A�v�A�v�A�v�A�v�A�v�A�v�A�v�A�x�A�x�A�z�A�x�A�v�A�t�A�jA�ffA�+A�JA���A�^5A���A���A�ZA��A��hA��uA�ZA��jA�l�A�~�A���A��jA�A��DA�`BA��A�A�A���A� �A��FA�\)A�{A��HA��A��A� �A�7LA��A��A�r�A���A��A�  A���A�-A��;A��7A��-A��mA��uA�XA��A�z�A�t�A��yA���A��hA�C�A��DA��9A��mA�-A�t�A��-A�E�A}�A{�Az�+AyK�Ax�yAw�Au�FAs�As�Ar(�Aq\)Ap��Ao�;Am|�AkAkAi��AhȴAf��Ae+Ac�TAb�uAadZA` �A_?}A^�jA]��A\�A[K�A[%AZ~�AY��AY\)AXZAW33AVZAU�7AU%AT�AR�AR=qAQ|�AP��AO��AN1AMO�AL�`AL��AL�DAKS�AK/AJ��AI�FAGdZAEp�AD�!AC��ACC�AB��AA��A@�HA@z�A?�#A>�RA=��A<jA;��A9��A8��A7�A69XA5��A5hsA4�DA2��A1dZA0�uA05?A/A/|�A/�A.�A.(�A-K�A,�jA,�\A,M�A+�
A+G�A*ZA)A)C�A)&�A(��A'�PA&=qA%l�A%7LA$�yA$ �A$�A#��A!7LA�wA�7AhsAS�A�A�yA��Az�A�mA��A�A|�A��A��A�;A?}A�/AĜA�A1A��A�DA1AȴAx�A~�A�A"�AM�A�A=qA��A��A7LA{A��AAhsA
��A	��A~�A�A1'A��A��A�A�;A (�@�n�@�9X@�ȴ@�5?@�@�X@���@��F@���@��
@�\)@��@�@��@�S�@�ȴ@�n�@�J@홚@��@��@�u@�A�@��@�u@�7L@��@��@�  @�K�@�~�@ݑh@���@��m@�J@�`B@��@ش9@�r�@�(�@ׅ@�E�@�J@���@�x�@�V@ԓu@҇+@�z�@�\)@�{@̼j@ˍP@��@�G�@��
@���@��@�  @�|�@�5?@�1'@�+@�V@��^@�r�@�$�@��T@�`B@�/@��j@���@���@�dZ@�M�@�p�@��P@���@�=q@���@���@�K�@���@�~�@�ff@���@�G�@�V@��@���@�S�@��H@�v�@�X@��j@���@���@�@���@�1'@���@�K�@�;d@���@�5?@���@��m@�ƨ@���@���@�5?@���@���@�\)@�;d@���@�~�@���@�x�@�`B@�G�@�/@���@�Z@���@�ȴ@�-@��@���@�`B@�V@�Ĝ@�(�@��P@�;d@���@�E�@�$�@��^@��h@�O�@��@���@�Z@��@�l�@���@��!@�V@��7@�G�@�%@���@��@���@��@�9X@�ƨ@�;d@�
=@��!@�v�@�5?@���@�/@��@���@�Z@� �@��
@���@��@��y@��@��\@�E�@�J@��^@��7@�?}@���@���@�I�@� �@���@���@�C�@�
=@��@��\@�E�@�@��@�G�@�V@���@���@�z�@�Q�@�9X@�1@|�@~��@~�@~�+@~{@}��@}V@|�D@{�m@z��@z~�@z�@y��@y�7@yG�@x�`@xbN@w�@w�w@w�P@w;d@v��@vff@u`B@t�/@t�D@tz�@tz�@tZ@t1@s�@r~�@qhs@p��@p�9@p�@pA�@p1'@p  @o��@o�@o��@o�P@o|�@o|�@ol�@o+@n��@nff@nV@n{@m`B@l��@l��@lz�@lI�@l�@k��@k�F@kt�@k33@k"�@k@j��@j�\@jn�@j-@i�#@ihs@i&�@h��@h�u@h�@hA�@g\)@f�@f�@f�R@fE�@e�T@e�@e`B@e?}@e/@e?}@d�/@dj@d(�@c��@b�@b��@b�\@b�\@bn�@b-@a�@a��@a��@ax�@a7L@a�@`Ĝ@`bN@_�w@_�P@_l�@_;d@_�@^�y@^��@^5?@^{@]@]�@]V@\z�@[��@[33@["�@["�@Z�!@Z=q@Y��@Y7L@Y%@Y%@X�`@XĜ@X�u@W�;@Wl�@W;d@W
=@Vȴ@V��@U��@UV@T��@T�@T�D@Tz�@Tz�@Tz�@Tj@T�@S�
@Sƨ@S�F@S��@R�!@RJ@QG�@Q&�@P��@P�u@PbN@O��@N�@N�+@Nff@M�@M@M�h@M`B@M/@M�@L��@K�
@K��@KS�@J�\@I�7@Ihs@IX@I�@H��@HA�@Gl�@G+@G�@F�y@F�y@F��@F�y@F�@Fv�@F5?@E�T@E�@EV@D��@D�/@D�@D�D@DZ@Cƨ@C��@C��@C��@B�@B-@Ahs@A&�@A�@@��@@b@?��@?|�@?
=@>V@=�T@=�@=?}@=V@<��@<�@<z�@<j@<j@<�@:��@:�@9%@8��@8Ĝ@8�9@8�u@81'@8b@7��@7�w@7�@7��@7l�@6��@6v�@65?@6$�@6$�@6@5@5��@5p�@4�@4z�@4Z@49X@3�m@3�F@3��@333@3@2��@2�\@2~�@2M�@2=q@1��@1X@0��@0�u@0A�@/�@/�@/�@.�y@.��@.ff@-�-@-V@,��@,j@,Z@,I�@,�@+��@+�
@+dZ@+33@+"�@+"�@+o@+@*�H@*�\@*M�@)��@)�@)�#@)��@)��@)�7@)X@)%@)%@)%@)%@(��@(��@(��@(�`@(��@(Ĝ@(�9@(��@(�u@(�@(bN@(b@(b@(b@'�@'��@'�@'��@'��@'��@'��@'��@'+@&�@&ȴ@&�R@&v�@&ff@&E�@%�@%�T@%��@%@%�-@%�h@%`B@%O�@%/@%V@$��@$�j@$�j@$�j@$�j@$�j@$�@$j@$9X@$(�@#�
@#��@#S�@#33@"�!@"J@!�^@!hs@!7L@!%@ Ĝ@ �9@ �9@ �9@ Ĝ@ �9@ ��@ bN@  �@ b@ b@�@l�@\)@K�@K�@+@+@
=@�R@ff@{@�T@�-@O�@�@�@�/@j@9X@��@�F@S�@�H@�!@�\@~�@n�@M�@=q@J@�#@��@�^@��@��@x�@%@�u@A�@1'@ �@b@�;@��@�@��@\)@�@��@v�@V@{@�-@�h@�h@�@�@`B@/@��@z�@I�@�@�m@�F@S�@�@��@�\@~�@=q@�#@�7@hs@��@�9@�u@�@�@r�@r�@r�@bN@bN@1'@�@��@;d@
=@�y@�+@E�@�@��@�-@�h@�@p�@`B@O�@O�@/@��@�@�/@�j@�@�F@33@o@o@@
��@
�!@
�\@
n�@
M�@
M�@
=q@
-@
-@
-@
�@	�#@	�^@	��@	hs@	&�@	�@	%@�9@b@�w@l�@+@
=@��@�y@�@ȴ@ȴ@ȴ@�R@�R@�R@��@��@v�@v�@5?@��@O�@�@�/@�@�@�/@�/@��@�@9X@�@1@��@�
@�@S�@33@�@��@~�@~�@n�@n�@n�@^5@M�@=q@-@J@�@��@��@�7@x�@x�@hs@hs@hs111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�S�A�ZA�VA�O�A�`BA�ffA�hsA�hsA�hsA�jA�hsA�jA�hsA�jA�l�A�n�A�p�A�p�A�t�A�t�A�v�A�v�A�v�A�v�A�v�A�v�A�v�A�x�A�x�A�z�A�x�A�v�A�t�A�jA�ffA�+A�JA���A�^5A���A���A�ZA��A��hA��uA�ZA��jA�l�A�~�A���A��jA�A��DA�`BA��A�A�A���A� �A��FA�\)A�{A��HA��A��A� �A�7LA��A��A�r�A���A��A�  A���A�-A��;A��7A��-A��mA��uA�XA��A�z�A�t�A��yA���A��hA�C�A��DA��9A��mA�-A�t�A��-A�E�A}�A{�Az�+AyK�Ax�yAw�Au�FAs�As�Ar(�Aq\)Ap��Ao�;Am|�AkAkAi��AhȴAf��Ae+Ac�TAb�uAadZA` �A_?}A^�jA]��A\�A[K�A[%AZ~�AY��AY\)AXZAW33AVZAU�7AU%AT�AR�AR=qAQ|�AP��AO��AN1AMO�AL�`AL��AL�DAKS�AK/AJ��AI�FAGdZAEp�AD�!AC��ACC�AB��AA��A@�HA@z�A?�#A>�RA=��A<jA;��A9��A8��A7�A69XA5��A5hsA4�DA2��A1dZA0�uA05?A/A/|�A/�A.�A.(�A-K�A,�jA,�\A,M�A+�
A+G�A*ZA)A)C�A)&�A(��A'�PA&=qA%l�A%7LA$�yA$ �A$�A#��A!7LA�wA�7AhsAS�A�A�yA��Az�A�mA��A�A|�A��A��A�;A?}A�/AĜA�A1A��A�DA1AȴAx�A~�A�A"�AM�A�A=qA��A��A7LA{A��AAhsA
��A	��A~�A�A1'A��A��A�A�;A (�@�n�@�9X@�ȴ@�5?@�@�X@���@��F@���@��
@�\)@��@�@��@�S�@�ȴ@�n�@�J@홚@��@��@�u@�A�@��@�u@�7L@��@��@�  @�K�@�~�@ݑh@���@��m@�J@�`B@��@ش9@�r�@�(�@ׅ@�E�@�J@���@�x�@�V@ԓu@҇+@�z�@�\)@�{@̼j@ˍP@��@�G�@��
@���@��@�  @�|�@�5?@�1'@�+@�V@��^@�r�@�$�@��T@�`B@�/@��j@���@���@�dZ@�M�@�p�@��P@���@�=q@���@���@�K�@���@�~�@�ff@���@�G�@�V@��@���@�S�@��H@�v�@�X@��j@���@���@�@���@�1'@���@�K�@�;d@���@�5?@���@��m@�ƨ@���@���@�5?@���@���@�\)@�;d@���@�~�@���@�x�@�`B@�G�@�/@���@�Z@���@�ȴ@�-@��@���@�`B@�V@�Ĝ@�(�@��P@�;d@���@�E�@�$�@��^@��h@�O�@��@���@�Z@��@�l�@���@��!@�V@��7@�G�@�%@���@��@���@��@�9X@�ƨ@�;d@�
=@��!@�v�@�5?@���@�/@��@���@�Z@� �@��
@���@��@��y@��@��\@�E�@�J@��^@��7@�?}@���@���@�I�@� �@���@���@�C�@�
=@��@��\@�E�@�@��@�G�@�V@���@���@�z�@�Q�@�9X@�1@|�@~��@~�@~�+@~{@}��@}V@|�D@{�m@z��@z~�@z�@y��@y�7@yG�@x�`@xbN@w�@w�w@w�P@w;d@v��@vff@u`B@t�/@t�D@tz�@tz�@tZ@t1@s�@r~�@qhs@p��@p�9@p�@pA�@p1'@p  @o��@o�@o��@o�P@o|�@o|�@ol�@o+@n��@nff@nV@n{@m`B@l��@l��@lz�@lI�@l�@k��@k�F@kt�@k33@k"�@k@j��@j�\@jn�@j-@i�#@ihs@i&�@h��@h�u@h�@hA�@g\)@f�@f�@f�R@fE�@e�T@e�@e`B@e?}@e/@e?}@d�/@dj@d(�@c��@b�@b��@b�\@b�\@bn�@b-@a�@a��@a��@ax�@a7L@a�@`Ĝ@`bN@_�w@_�P@_l�@_;d@_�@^�y@^��@^5?@^{@]@]�@]V@\z�@[��@[33@["�@["�@Z�!@Z=q@Y��@Y7L@Y%@Y%@X�`@XĜ@X�u@W�;@Wl�@W;d@W
=@Vȴ@V��@U��@UV@T��@T�@T�D@Tz�@Tz�@Tz�@Tj@T�@S�
@Sƨ@S�F@S��@R�!@RJ@QG�@Q&�@P��@P�u@PbN@O��@N�@N�+@Nff@M�@M@M�h@M`B@M/@M�@L��@K�
@K��@KS�@J�\@I�7@Ihs@IX@I�@H��@HA�@Gl�@G+@G�@F�y@F�y@F��@F�y@F�@Fv�@F5?@E�T@E�@EV@D��@D�/@D�@D�D@DZ@Cƨ@C��@C��@C��@B�@B-@Ahs@A&�@A�@@��@@b@?��@?|�@?
=@>V@=�T@=�@=?}@=V@<��@<�@<z�@<j@<j@<�@:��@:�@9%@8��@8Ĝ@8�9@8�u@81'@8b@7��@7�w@7�@7��@7l�@6��@6v�@65?@6$�@6$�@6@5@5��@5p�@4�@4z�@4Z@49X@3�m@3�F@3��@333@3@2��@2�\@2~�@2M�@2=q@1��@1X@0��@0�u@0A�@/�@/�@/�@.�y@.��@.ff@-�-@-V@,��@,j@,Z@,I�@,�@+��@+�
@+dZ@+33@+"�@+"�@+o@+@*�H@*�\@*M�@)��@)�@)�#@)��@)��@)�7@)X@)%@)%@)%@)%@(��@(��@(��@(�`@(��@(Ĝ@(�9@(��@(�u@(�@(bN@(b@(b@(b@'�@'��@'�@'��@'��@'��@'��@'��@'+@&�@&ȴ@&�R@&v�@&ff@&E�@%�@%�T@%��@%@%�-@%�h@%`B@%O�@%/@%V@$��@$�j@$�j@$�j@$�j@$�j@$�@$j@$9X@$(�@#�
@#��@#S�@#33@"�!@"J@!�^@!hs@!7L@!%@ Ĝ@ �9@ �9@ �9@ Ĝ@ �9@ ��@ bN@  �@ b@ b@�@l�@\)@K�@K�@+@+@
=@�R@ff@{@�T@�-@O�@�@�@�/@j@9X@��@�F@S�@�H@�!@�\@~�@n�@M�@=q@J@�#@��@�^@��@��@x�@%@�u@A�@1'@ �@b@�;@��@�@��@\)@�@��@v�@V@{@�-@�h@�h@�@�@`B@/@��@z�@I�@�@�m@�F@S�@�@��@�\@~�@=q@�#@�7@hs@��@�9@�u@�@�@r�@r�@r�@bN@bN@1'@�@��@;d@
=@�y@�+@E�@�@��@�-@�h@�@p�@`B@O�@O�@/@��@�@�/@�j@�@�F@33@o@o@@
��@
�!@
�\@
n�@
M�@
M�@
=q@
-@
-@
-@
�@	�#@	�^@	��@	hs@	&�@	�@	%@�9@b@�w@l�@+@
=@��@�y@�@ȴ@ȴ@ȴ@�R@�R@�R@��@��@v�@v�@5?@��@O�@�@�/@�@�@�/@�/@��@�@9X@�@1@��@�
@�@S�@33@�@��@~�@~�@n�@n�@n�@^5@M�@=q@-@J@�@��@��@�7@x�@x�@hs@hs@hs111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B�ZB�TB�TB�ZB�TB�TB�TB�TB�TB�TB�ZB�TB�TB�TB�TB�TB�TB�TB�TB�TB�TB�TB�TB�ZB�ZB�ZB�ZB�ZB�ZB�ZB�ZB�ZB�ZB�ZB�TB�HB�)B��B��B�PB�7B�B�B|�Bx�Bo�Bl�BjBffBdZBYBN�BF�B<jB9XB49B0!B+B%�B"�B�BoB��B�B�`B�
BĜB��B��B�{B�DB�=B�1B� By�Bs�BgmB\)BE�B?}B7LB/B�B�BhBB
�B
�NB
�B
ȴB
�qB
�-B
��B
��B
�+B
p�B
hsB
`BB
\)B
P�B
F�B
9XB
2-B
+B
#�B
�B
�B
1B	��B	��B	�B	�mB	�B	��B	��B	ɺB	B	�XB	�3B	�B	��B	��B	��B	��B	��B	�uB	�bB	�=B	�B	}�B	x�B	u�B	p�B	jB	gmB	cTB	_;B	ZB	Q�B	N�B	L�B	K�B	I�B	C�B	B�B	?}B	8RB	/B	$�B	!�B	�B	�B	�B	uB	bB	VB	DB	%B	B��B��B�B�B�sB�TB�HB�;B�#B��B��BƨBĜBB��B�}B�qB�jB�jB�^B�XB�LB�?B�3B�B�B�B�B��B��B��B��B��B��B��B��B��B�VB�7B�1B�1B�1B�+B�%B�B�B�B�B�B�B� B~�B|�Bz�Bx�Bw�Bv�Bt�Bs�Bp�Bm�BjBgmBdZBbNB_;B]/BZBYBW
BVBT�BQ�BO�BL�BK�BI�BF�BC�BA�B?}B>wB<jB:^B8RB6FB5?B49B33B33B2-B2-B1'B0!B/B/B.B.B-B,B,B,B,B+B+B+B+B)�B)�B'�B'�B'�B'�B&�B'�B'�B&�B&�B&�B&�B(�B(�B(�B)�B)�B(�B(�B+B+B+B+B)�B)�B+B-B-B/B/B0!B0!B1'B1'B1'B1'B0!B0!B1'B5?B49B33B2-B/B0!B1'B2-B33B7LB9XB:^B;dB=qB=qB@�BA�BA�BA�BA�BC�BE�BF�BH�BL�BO�BQ�BP�BP�BQ�BR�BT�B]/B^5B^5B_;B_;B`BBdZBe`BgmBgmBjBl�Bn�Bq�Bq�Br�Bt�Bv�Bz�B�B�%B�%B�+B�=B�\B�hB�oB�uB�{B��B��B��B��B��B��B��B��B��B��B�B�B�B�'B�3B�9B�?B�FB�LB�RB�^B�dB�jB��BŢBŢBŢBȴB��B��B��B��B��B��B��B��B�B�B�#B�/B�/B�HB�TB�`B�mB�yB�B�B�B�B�B��B��B��B��B��B��B	  B	B	B	+B	1B		7B	PB	\B	hB	uB	�B	�B	�B	�B	�B	!�B	$�B	&�B	'�B	)�B	+B	-B	/B	2-B	2-B	33B	5?B	7LB	9XB	:^B	=qB	@�B	A�B	C�B	D�B	E�B	F�B	H�B	K�B	L�B	M�B	N�B	O�B	P�B	R�B	XB	[#B	]/B	^5B	`BB	dZB	e`B	ffB	hsB	jB	k�B	l�B	m�B	m�B	n�B	q�B	r�B	t�B	t�B	u�B	u�B	v�B	v�B	w�B	y�B	z�B	{�B	|�B	�B	�B	�B	�+B	�1B	�=B	�DB	�JB	�VB	�\B	�\B	�\B	�\B	�hB	�oB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�-B	�3B	�9B	�9B	�9B	�?B	�LB	�LB	�RB	�XB	�^B	�dB	�jB	�wB	��B	B	ÖB	ĜB	ŢB	ŢB	ŢB	ǮB	ȴB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�#B	�#B	�)B	�)B	�/B	�BB	�HB	�HB	�NB	�NB	�NB	�NB	�NB	�TB	�ZB	�ZB	�ZB	�ZB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
1B
	7B
	7B
	7B

=B
DB
JB
JB
PB
PB
PB
VB
\B
bB
bB
bB
hB
uB
{B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
!�B
!�B
"�B
#�B
#�B
#�B
$�B
$�B
$�B
$�B
%�B
&�B
'�B
'�B
'�B
'�B
(�B
(�B
(�B
)�B
+B
+B
+B
,B
-B
-B
-B
.B
.B
.B
.B
/B
/B
0!B
0!B
1'B
2-B
2-B
33B
33B
49B
5?B
5?B
5?B
6FB
7LB
8RB
9XB
9XB
9XB
9XB
9XB
:^B
:^B
;dB
;dB
;dB
;dB
;dB
;dB
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
?}B
>wB
?}B
?}B
?}B
?}B
?}B
?}B
?}B
?}B
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
C�B
B�B
C�B
C�B
C�B
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
F�B
G�B
G�B
G�B
H�B
H�B
I�B
I�B
J�B
J�B
K�B
K�B
K�B
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
R�B
R�B
S�B
T�B
T�B
T�B
VB
VB
VB
VB
W
B
XB
XB
XB
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
ZB
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
^5B
^5B
^5B
^5B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
`BB
`BB
aHB
aHB
bNB
bNB
bNB
cTB
cTB
dZB
dZB
dZB
dZB
e`B
e`B
ffB
ffB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
hsB
hsB
hsB
iyB
iyB
iyB
jB
jB
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
m�B
m�B
n�B
n�B
n�B
n�B
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
p�B
p�B
p�B
p�B
q�B
q�B
q�B
q�B
q�B
r�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
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
t�B
s�B
s�B
t�B
t�B
t�B
t�B
t�B
u�B
u�B
u�B
u�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
w�B
w�B
w�B
w�B
x�B
x�B
x�B
x�B
x�B
x�B
x�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B�ZB�TB�TB�@B�TB�TB�TB�TB�TB�TB�ZB�TB�TB�TB�TB�TB�TB�TB�TB�TB�TB�TB�TB�ZB�ZB�ZB�ZB�ZB�ZB�ZB�ZB�tB�B��B�tB��B�BݲB��B�HB��B��B�SB�B|�Bq'Bm�Bl�BhsBffB[=BRBIB=�B:�B5tB1�B,B&�B$@B!�BB�B�B��B�7BȀB�sB�5B��B��B��B��B��B{Bu�Bi�B^�BFtB@�B8�B1'B!-B�BB�B
�B
�ZB
��B
ʌB
�HB
�B
��B
��B
��B
rB
i�B
aHB
^5B
R�B
H�B
:xB
3MB
,B
$�B
;B
1B

#B	�"B	�LB	�[B	��B	�B	өB	уB	�DB	��B	�xB	�B	�iB	�B	�B	�CB	�kB	�sB	�FB	��B	��B	�3B	~�B	y�B	v�B	rB	kkB	hsB	d�B	`�B	[�B	R�B	O\B	MB	L~B	J�B	DB	CaB	AUB	;B	1AB	&B	#B	IB	�B	�B	{B	B	vB	�B	�B	�B	 iB��B�9B�!B�B�B�B�B�B��B��B�EB�B�B�'B�OB�(B�qB�"B��B��B�B�+B�TB��B��B�qB��B��B�zB��B�!B�OB��B�B��B�KB��B��B��B�fB��B��B��B��B��B�aB�[B�uB��B��B� B}�B{dBy$BxRBw�Bu�Bu%Bq�BoOBl=Bh�Be`BcnB`�B^�BZ�BY�BWsBV�BV�BS�BP�BM�BMBKDBHfBE�BB�B@�B@�B=�B<B:xB7�B6�B5%B3�B3�B2�B2�B2GB1�B/�B/�B.}B.�B-�B-]B,qB,WB,qB+kB+kB+QB+kB*�B+6B)�B*B)_B(�B(>B(�B(�B'�B'�B'�B(
B)yB)_B)DB*0B*eB)�B)�B+QB+QB+QB+�B*�B+�B,WB-�B.B0!B0B0�B1AB2-B2B2GB1�B0�B1AB2|B5�B4�B3�B3MB0oB0oB1�B2|B3�B7�B9�B:�B<6B>BB>�BA BA�BB'BBABB�BC�BE�BF�BI7BMBO�BR BQhBQ�BRTBSuBU�B]�B^�B_B_�B_�B`�Bd�Be�Bg�Bg�BkBm)Bo�Bq�Bq�BsMBuZBw�B|B�aB�YB�tB��B��B��B�hB��B��B��B�B�7B�CB�!B�B�B�@B�2B�>B�yB��B�cB��B�vB�hB��B�ZB��B��B��B��B��B��B��B��B�B�%B��B��B��B��B��B�(B�4B�[B�MB�EB�eB�qB�~BݘB�B�B�B�B�B�B��B� B��B��B�B�B�$B�*B�B�BB	 OB	[B	gB	_B	fB		�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	!�B	%B	'B	($B	)�B	+6B	-CB	/OB	2GB	2aB	3hB	5tB	7�B	9�B	:�B	=�B	@�B	A�B	C�B	D�B	E�B	F�B	H�B	K�B	L�B	M�B	N�B	O�B	Q4B	S[B	XEB	[=B	]IB	^OB	`\B	d�B	e�B	f�B	h�B	j�B	k�B	l�B	m�B	m�B	n�B	q�B	r�B	t�B	t�B	u�B	u�B	v�B	v�B	xB	y�B	z�B	|B	}<B	�;B	�B	�B	�EB	�KB	�XB	�^B	�dB	�pB	�vB	�vB	�vB	�\B	��B	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�*B	�0B	�"B	�CB	�UB	�GB	�MB	�9B	�TB	�TB	�ZB	�fB	�fB	�lB	�rB	�xB	��B	��B	��B	��B	ªB	ðB	ĜB	żB	żB	��B	��B	��B	��B	��B	�B	�B	��B	��B	�B	� B	�&B	�,B	�,B	�B	��B	�B	�9B	�$B	�EB	�1B	�7B	�=B	�#B	�CB	�xB	�~B	�\B	�HB	�HB	�NB	�NB	�NB	�hB	�hB	�nB	�B	�tB	�B	��B	�B	�B	�B	�B	��B	�B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�	B	�*B	�*B	�B	�B	�B	�"B	�BB
 OB
B
'B
-B
�B
B
B
KB
	RB
	RB
	RB

rB
^B
0B
dB
jB
PB
jB
�B
\B
}B
}B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
B
B
 B
!�B
!�B
!�B
!�B
"�B
#�B
#�B
#�B
$�B
%B
$�B
%B
&B
'B
'�B
'�B
(
B
(
B
)B
)B
)*B
*0B
+B
+B
+B
,"B
-)B
-)B
-)B
./B
.IB
./B
./B
/5B
/OB
0oB
0UB
1AB
2GB
2GB
3MB
3hB
49B
5ZB
5tB
5�B
6zB
7�B
8lB
9XB
9rB
9rB
9rB
9rB
:�B
:xB
;dB
;dB
;dB
;B
;dB
;B
<�B
<jB
=qB
=qB
=qB
=�B
>�B
>�B
>�B
>wB
?}B
>wB
?}B
?}B
?}B
?}B
?�B
?}B
?}B
?}B
?}B
?�B
?�B
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
BuB
B�B
B�B
C�B
B�B
C�B
C�B
C�B
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
F�B
G�B
G�B
G�B
H�B
H�B
I�B
I�B
J�B
J�B
K�B
K�B
K�B
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
PB
P�B
P�B
P�B
Q B
P�B
Q B
P�B
Q B
RB
RB
SB
S&B
TB
T�B
UB
UB
VB
VB
VB
VB
W$B
X+B
X+B
XEB
YB
Y1B
YB
Z7B
Z7B
ZB
ZB
[#B
Z7B
[=B
ZQB
[WB
[=B
\)B
\)B
\CB
\)B
\B
\CB
\CB
\CB
]dB
^OB
^5B
^OB
^OB
_;B
_VB
_;B
_;B
_;B
_VB
_pB
`\B
`\B
abB
abB
b�B
b�B
b�B
cnB
cTB
dZB
dtB
dtB
d�B
e`B
ezB
f�B
f�B
gmB
gmB
gmB
gmB
gmB
gmB
gRB
gmB
g�B
g�B
h�B
h�B
h�B
i�B
i�B
i�B
j�B
j�B
jB
k�B
k�B
kkB
k�B
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
n�B
n}B
n�B
n�B
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
p�B
p�B
p�B
p�B
q�B
q�B
q�B
q�B
q�B
r�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
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
t�B
s�B
s�B
t�B
t�B
t�B
t�B
t�B
u�B
u�B
u�B
u�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
w�B
w�B
w�B
w�B
x�B
x�B
x�B
x�B
x�B
x�B
x�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<��P<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=-0.01(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201912200032042019122000320420191220003204202211182141152022111821411520221118214115201912210018532019122100185320191221001853  JA  ARFMdecpA19c                                                                20191210003713  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20191209153715  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20191209153717  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20191209153718  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20191209153718  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20191209153718  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20191209153719  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcp2.8e                                                                20191209153719  QCF$                G�O�G�O�G�O�            8000JA  ARGQaqcp2.8e                                                                20191209153719  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20191209153719  QCF$                G�O�G�O�G�O�            8000JA  ARGQrqcpt16c                                                                20191209153719  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20191209153720                      G�O�G�O�G�O�                JA  ARUP                                                                        20191209155335                      G�O�G�O�G�O�                JM  ARGQrqcjv291                                                                20191209153323  QCP$                G�O�G�O�G�O�2DEB7C          JM  ARGQJMQC2.0                                                                 20191209153243  CV  JULD            G�O�G�O�FǑ                 JM  ARCAJMQC2.0                                                                 20191219153204  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20191219153204  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20191220151853  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20221118124115  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20221123111508                      G�O�G�O�G�O�                