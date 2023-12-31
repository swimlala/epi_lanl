CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2020-06-19T17:08:53Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         ZPRIMARY | https://orcid.org/0000-0001-7324-3159 | Matthew Alkire, University of Washington        @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    7   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    7(   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    7,   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    70   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7@   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7P   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7`   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7h   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7�   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        8   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    8   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    8    DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     8$   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8D   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8H   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8L   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8l   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8�   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8�   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           8�   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
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
resolution        :�o     �  q�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    y�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �T   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �d   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �h   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �x   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �|   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20200619170853  20220204114411  5905979 UW, Argo                                                        STEPHEN RISER                                                   PRES            TEMP            PSAL               
A   AO  7662                            2C  D   APEX                            8312                            080318                          846 @؄<}�1   @؄��h^@7@     �c���n�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    
A   B   B   @���@�  A   AffA>ffA^ffA�  A�  A�  A�  A�  A�  A�  A�  A�33B��B  B  B   B'��B0  B8  B@ffBH  BP  BW��B`  Bh  Bp��Bw��B��B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C'�fC*  C,  C.�C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C��C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D.��D/� D0fD0�fD1fD1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^�fD_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt��Dy��D��D�g
D���D�߮D�{D�NfD���D�� D��D�_�D��=D��3D��D�W�D�S�D���D��D�W
D�fD��
111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@�Q�A (�A�\A>�\A^�\A�{A�{A�{A�{A�{A�{A�{A�{A�G�B��B
=B
=B 
=B'��B0
=B8
=B@p�BH
=BP
=BW��B`
=Bh
=Bp�
Bw��B��B���B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�C �C�C�C�C�C
�C�C�C�C�C�C�C�C�C�C�C �C"�C$�C&�C'��C*�C,�C.)C0�C2�C4�C6�C8�C:�C<�C>�C@�CB�CD�CF�CH�CJ�CL�CN�CP�CR�CT�CV�CX�CZ�C\�C^�C`�Cb�Cd�Cf�Ch�Cj�Cl�Cn�Cp�Cr�Ct�Cv�Cx�Cz�C|�C~�C�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC��{C�HC�C�HC��{C�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC��{C�HC�HC�C�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HD  �D ��D �D��D �D��D �D��D �D��D �D��D �D�
D �D��D �D��D	 �D	��D
 �D
��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D  �D ��D! �D!��D" �D"��D# �D#��D$ �D$��D% �D%��D& �D&��D' �D'��D( �D(��D) �D)��D* �D*��D+ �D+��D, �D,��D- �D-��D. �D.��D.�>D/��D0
D0�
D1
D1��D2 �D2��D3 �D3��D4 �D4��D5 �D5��D6 �D6��D7 �D7��D8 �D8��D9 �D9��D: �D:��D; �D;��D< �D<��D= �D=��D> �D>��D? �D?��D@ �D@��DA �DA��DB �DB��DC �DC��DD �DD��DE �DE��DF �DF��DG �DG��DH �DH��DI �DI��DJ �DJ��DK �DK��DL �DL��DM �DM��DN �DN��DO �DO��DP �DP��DQ �DQ��DR �DR��DS �DS��DT �DT��DU �DU��DV �DV��DW �DW��DX �DX��DY �DY��DZ �DZ��D[ �D[��D\ �D\��D] �D]��D^ �D^�
D_ �D_��D` �D`��Da �Da��Db �Db��Dc �Dc��Dd �Dd��De �De��Df �Df��Dg �Dg��Dh �Dh��Di �Di��Dj �Dj��Dk �Dk��Dl �Dl��Dm �Dm��Dn �Dn��Do �Do��Dp �Dp��Dq �Dq��Dr �Dr��Ds �Ds��Dt �Dt��Dt�qDy��D��D�g\D��D�� D��D�N�D���D��RD�)D�` D���D��D�3D�X D�S�D��)D�D�W\D�D��\111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�VA�VA�XA�VA�VA�XA�XA�VA�XA�ZA�XA�M�A�5?A�33A�1'A��A���A��yAӾwA�ƨA��;A��AӬA�v�A�%A���A�XAʉ7A��A��HA�z�A��A��+A���A�A�JA�33A���A���A���A���A��TA�`BA��A�I�A���A�%A�ĜA�JA�ffA���A��/A�z�A�&�A��jA���A�1A��A��#A�x�A���A�=qA�ƨA�E�A�`BA�%A��TA��DA�bNA��9A�K�A��\A�7LA��A���A���A��A��RA��A�
=A�^5A�n�A�Q�A�VA�~�A���A���A��A�K�A�A�z�A�A�O�A��A�K�A��A���A�A��RA���A��A�VA�Q�A���A�l�Al�A}�A|��A{��Au�PArz�Ap�uAn �Alv�Al�AkS�Aj��Ai�TAhM�AgS�Af=qAe�Ab��A`�`A_�A]��A\JAZ^5AYhsAX(�AW�AW�hAV�AUp�AT�\ASC�AQ�wAQXAP��AN�AN=qAMhsAMVAL��AK;dAH�`AFĜAE��AE&�AD��AD1'AC��ACXAC�AB��AB�AA�A?|�A>�\A=�^A=O�A<z�A;%A9��A8�yA7XA6ZA6�A5��A5ƨA5|�A4��A4��A3�-A2��A2n�A1�A1A0z�A/\)A-�A-l�A-�A,z�A+dZA*��A)G�A(Q�A&9XA$��A$5?A#�^A"�yA"�A!��A!?}A �`A M�A��A��A�^A��A�wA�HA{A\)AA�HA�-A=qA�9A��A�A��A$�A��A�mA|�A�A��A��AA	��A	33A-A;dAZA�
A��A�AK�A��A$�A�A -@�n�@�(�@���@�Z@��;@��P@�
=@�n�@���@��@�7@���@�@�w@�\@�7@�j@��@�z�@� �@柾@�9X@�"�@���@�v�@��@�A�@��@�?}@��
@��#@؛�@ם�@�o@և+@�=q@���@�E�@�9X@·+@�X@˾w@�^5@��@�7L@�1'@��@ǍP@��#@�x�@�hs@�X@�?}@�V@�Z@��@�$�@��-@�`B@�V@��u@���@���@���@��F@��H@��@��@��@���@�l�@���@�5?@��#@��@��`@���@�~�@�&�@�%@��@��@�"�@�`B@�A�@�;d@��@�ff@��#@��-@�hs@�O�@��7@�V@��@�
=@�n�@�@��-@�V@�A�@�ƨ@�\)@��H@��\@��T@�x�@�O�@��@� �@��F@�C�@��@���@��R@�v�@��T@��7@��@��@��@��@��9@�r�@��@�r�@�j@�z�@�1@��P@�dZ@��R@�-@�J@�n�@�E�@�{@�{@�@�@�@�@���@�@���@��@�&�@�Ĝ@�  @��F@���@�;d@�
=@���@�-@��@���@��h@��@���@��F@���@���@��w@���@�@���@��!@���@�v�@�ff@��@�@��@��@��H@��@�5?@��@��^@�O�@��@��@���@��@�(�@��@��;@��F@�|�@�|�@�t�@��@��@�%@��@���@��9@���@���@�j@��u@���@���@��@�t�@�@�5?@��@��-@��h@��@�x�@���@���@��-@��h@�p�@��@�@���@��^@���@��h@��7@�x�@�hs@�X@�O�@�?}@�p�@��@���@��9@��u@�z�@�bN@�Z@�I�@�A�@�I�@�A�@� �@��@��w@��@�|�@�33@�
=@���@��!@���@��\@�v�@�Ɇ@�C�@s�@k.I@b �@Z�]@UL�@OS�@HFt@@�@9��@4�@.:*@(�@!��@�'@�U@��@5�@~@j111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A�VA�VA�XA�VA�VA�XA�XA�VA�XA�ZA�XA�M�A�5?A�33A�1'A��A���A��yAӾwA�ƨA��;A��AӬA�v�A�%A���A�XAʉ7A��A��HA�z�A��A��+A���A�A�JA�33A���A���A���A���A��TA�`BA��A�I�A���A�%A�ĜA�JA�ffA���A��/A�z�A�&�A��jA���A�1A��A��#A�x�A���A�=qA�ƨA�E�A�`BA�%A��TA��DA�bNA��9A�K�A��\A�7LA��A���A���A��A��RA��A�
=A�^5A�n�A�Q�A�VA�~�A���A���A��A�K�A�A�z�A�A�O�A��A�K�A��A���A�A��RA���A��A�VA�Q�A���A�l�Al�A}�A|��A{��Au�PArz�Ap�uAn �Alv�Al�AkS�Aj��Ai�TAhM�AgS�Af=qAe�Ab��A`�`A_�A]��A\JAZ^5AYhsAX(�AW�AW�hAV�AUp�AT�\ASC�AQ�wAQXAP��AN�AN=qAMhsAMVAL��AK;dAH�`AFĜAE��AE&�AD��AD1'AC��ACXAC�AB��AB�AA�A?|�A>�\A=�^A=O�A<z�A;%A9��A8�yA7XA6ZA6�A5��A5ƨA5|�A4��A4��A3�-A2��A2n�A1�A1A0z�A/\)A-�A-l�A-�A,z�A+dZA*��A)G�A(Q�A&9XA$��A$5?A#�^A"�yA"�A!��A!?}A �`A M�A��A��A�^A��A�wA�HA{A\)AA�HA�-A=qA�9A��A�A��A$�A��A�mA|�A�A��A��AA	��A	33A-A;dAZA�
A��A�AK�A��A$�A�A -@�n�@�(�@���@�Z@��;@��P@�
=@�n�@���@��@�7@���@�@�w@�\@�7@�j@��@�z�@� �@柾@�9X@�"�@���@�v�@��@�A�@��@�?}@��
@��#@؛�@ם�@�o@և+@�=q@���@�E�@�9X@·+@�X@˾w@�^5@��@�7L@�1'@��@ǍP@��#@�x�@�hs@�X@�?}@�V@�Z@��@�$�@��-@�`B@�V@��u@���@���@���@��F@��H@��@��@��@���@�l�@���@�5?@��#@��@��`@���@�~�@�&�@�%@��@��@�"�@�`B@�A�@�;d@��@�ff@��#@��-@�hs@�O�@��7@�V@��@�
=@�n�@�@��-@�V@�A�@�ƨ@�\)@��H@��\@��T@�x�@�O�@��@� �@��F@�C�@��@���@��R@�v�@��T@��7@��@��@��@��@��9@�r�@��@�r�@�j@�z�@�1@��P@�dZ@��R@�-@�J@�n�@�E�@�{@�{@�@�@�@�@���@�@���@��@�&�@�Ĝ@�  @��F@���@�;d@�
=@���@�-@��@���@��h@��@���@��F@���@���@��w@���@�@���@��!@���@�v�@�ff@��@�@��@��@��H@��@�5?@��@��^@�O�@��@��@���@��@�(�@��@��;@��F@�|�@�|�@�t�@��@��@�%@��@���@��9@���@���@�j@��u@���@���@��@�t�@�@�5?@��@��-@��h@��@�x�@���@���@��-@��h@�p�@��@�@���@��^@���@��h@��7@�x�@�hs@�X@�O�@�?}@�p�@��@���@��9@��u@�z�@�bN@�Z@�I�@�A�@�I�@�A�@� �@��@��w@��@�|�@�33@�
=@���@��!@���@��\G�O�@�Ɇ@�C�@s�@k.I@b �@Z�]@UL�@OS�@HFt@@�@9��@4�@.:*@(�@!��@�'@�U@��@5�@~@j111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�RB�RB�RB�RB�RB�RB�RB�RB�RB�RB�RB�RB�LB�LB�LB�FB�FB�jB�`B��BDB�B(�B?}B:^B-Bk�B��B��B�!B�?BŢB��B��B��B�B�B�B��B��BǮBB�XB�?B�'B�!B�B��B��B��B��B�uB�bB�VB�JB�1B� B}�B{�Bx�Bq�BiyBcTB\)BR�B<jB9XB33B"�B�BDB+BPB
=BDB��B�HB�B��B�dB��B�oB�uB�hB�7Bw�BcTBI�B49BJBB
��B
�B
�B
�TB
��B
ɺB
��B
�dB
�RB
�'B
��B
�\B
�7B
�B
{�B
q�B
hsB
bNB
F�B
-B
 �B
�B
1B
%B
B	��B	��B	�B	�B	�fB	�5B	��B	ŢB	�qB	�9B	��B	��B	��B	�\B	�PB	�7B	�B	{�B	u�B	s�B	gmB	dZB	aHB	XB	R�B	M�B	I�B	G�B	@�B	6FB	&�B	�B	�B	�B	{B	bB	bB	VB	PB	DB		7B	B��B��B��B�B�B�sB�`B�HB�)B�#B�B�B�B�
B��B��B��B��BɺBȴBŢBÖB�wB�dB�^B�LB�3B�!B�B�B��B��B��B��B��B��B��B��B��B��B�uB�hB�JB�=B�+B�B�B� B|�B{�By�Bu�Bq�Bm�BjBgmBdZB]/BZBZBXBS�BQ�BQ�BN�BK�BJ�BG�BE�BD�BC�BA�B>wB>wB=qB9XB9XB7LB7LB5?B5?B49B49B33B2-B1'B1'B0!B0!B0!B0!B0!B.B.B-B.B/B1'B49B5?B6FB7LB8RB;dB;dB:^B<jB?}BA�BC�BC�BD�BC�BE�BF�BG�BK�BO�BR�BT�BT�BVBW
BW
BXB\)B[#B[#B[#B[#B\)B]/B`BB`BBaHBaHBbNBcTBdZBe`BhsBhsBiyBk�Bn�Bp�Bp�Bq�Bs�Bt�Bu�Bu�Bv�Bz�B}�B�B�B�B�7B�DB�=B�VB�hB�oB�uB��B��B��B��B��B��B��B��B��B��B��B��B�B�-B�9B�FB�FB�LB�RB�RB�^B��BĜBƨBǮBȴBɺB��B��B��B��B�#B�HB�yB�B�B��B��B��B��B	  B	B	B	B		7B	PB	oB	{B	�B	�B	�B	�B	 �B	!�B	!�B	$�B	'�B	+B	/B	1'B	0!B	0!B	33B	7LB	9XB	=qB	A�B	E�B	J�B	L�B	I�B	M�B	P�B	R�B	XB	]/B	_;B	bNB	bNB	bNB	dZB	e`B	hsB	o�B	r�B	s�B	t�B	w�B	w�B	y�B	}�B	� B	�B	�%B	�%B	�+B	�JB	�\B	�bB	�hB	�uB	�{B	��B	��B	��B	�uB	�oB	�oB	�oB	�oB	�uB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�!B	�!B	�-B	�FB	�RB	�^B	�dB	�dB	�jB	�jB	�jB	�jB	�jB	�qB	��B	��B	ÖB	ĜB	ƨB	ǮB	ȴB	ȴB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
?B
�B
�B
!HB
)_B
0UB
:B
=qB
E�B
L0B
Q4B
X�B
^B
b�B
f�B
jB
o B
rB
u111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�(B�(B�(B�"B�"B�FB�9B�BBoB�B6PB11B#�BbVB�UB��B��B�B�nBǱBɾB��B��B��B��BƬBÛB�|B�^B�(B�B��B��B��B��B�xB�lB�mB�HB�6B�*B�BBv�Bt�Br�Bo�Bh�B`PBZ+BSBI�B3EB03B*B�B�B"B�
B/BB#B��B�*B��BķB�IB��B�WB�]B�PB� Bn�BZ@B@�B+)B=B
�B
��B
�B
�B
�JB
��B
��B
�|B
�]B
�LB
�!B
��B
�YB
�4B
zB
r�B
h�B
_sB
YOB
=�B
$B
�B
�B	�9B	�-B	�B	�B	��B	�B	�B	�qB	�AB	��B	��B	�B	�HB	�B	��B	��B	�nB	�bB	�JB	z%B	r�B	l�B	j�B	^�B	[pB	X^B	O'B	J	B	D�B	@�B	>�B	7�B	-`B	B	�B	�B	�B	�B	B	B	sB	mB	aB	 UB�*B� B��B��B��B�BߔB܁B�iB�KB�EB�?B�9B�9B�,B� B�B��B��B��B��B��B��B��B��B��B�qB�YB�GB�:B�.B�B��B��B��B��B��B��B��B��B��B��B��B�tB�gB~UB|JBx1Bw+BtBsBqBl�Bh�Bd�Ba�B^�B[�BT^BQLBQLBO?BK(BIBIBF	BB�BA�B>�B<�B;�B:�B8�B5�B5�B4�B0�B0�B.B.B,rB,rB+lB+lB*gB)aB([B([B'UB'UB'UB'UB'UB%HB%IB$CB%IB&PB(\B+nB,tB-{B.�B/�B2�B2�B1�B3�B6�B8�B:�B:�B;�B:�B<�B=�B>�BB�BGBJ&BL2BL2BM8BN>BN>BODBS]BRWBRWBRWBRWBS]BTcBWvBWvBX|BX|BY�BZ�B[�B\�B_�B_�B`�Bb�Be�Bg�Bg�Bh�Bj�Bk�Bl�Bl�Bm�BrBu'B{KBzDB{KB�iB�vB�oB��B��B��B��B��B��B��B��B�B� B�&B�'B�B�B�!B�-B�>B�]B�iB�vB�vB�|B��B��B��B��B��B��B��B��B��B��B�B� B�,B�QB�uB�B��B��B��B��B��B�B�+B�1B�=B�JB	 bB	{B		�B	�B	�B	�B	�B	�B	�B	�B	�B	B	B	"+B	&CB	(OB	'IB	'IB	*[B	.tB	0�B	4�B	8�B	<�B	A�B	C�B	@�B	D�B	HB	JB	O6B	TUB	VaB	YsB	YsB	YsB	[B	\�B	_�B	f�B	i�B	j�B	k�B	n�B	n�B	p�B	uB	w$B	z5B	}HB	}HB	~NB	�mB	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�#B	�5B	�BB	�BB	�BB	�NB	�fB	�rB	�~B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	�B	�B	�B	�B	�B	�G�O�B	��B	�B	�[B
�B
�B
cB
 zB
'pB
1*B
4�B
<�B
CJB
HNB
O�B
UB
Y�B
]�B
a�B
fB
i,B
l$111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =-0.01 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            r =0.9998(+/-0.0001), vertically averaged dS =-0.009(+/-0.003) in PSS-78.                                                                                                                                                                                       Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                      202202041144112022020411441120220204114411  AO  ARCAADJP                                                                    20200619170853    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20200619170853  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20200619170853  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20220204114411  IP                  G�O�G�O�G�O�                