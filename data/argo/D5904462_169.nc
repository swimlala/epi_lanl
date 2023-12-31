CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-11-21T12:59:53Z creation      
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
_FillValue                    �,Argo profile    3.1 1.2 19500101000000  20181121125953  20190405100759  5904462 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  5287                            2C  D   APEX                            6529                            072314                          846 @���i��1   @���q�@0%�Q��d/\(�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   A   A   @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B���B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D*��D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dys3D�fD�P D��fD�� D�� D�Y�D��fD��3D� D�I�D���D�ɚD�	�D�C3Dډ�D�� D���D�C3D�l�D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�Q�@�Q�A(�A((�AH(�Ah(�A�{A�{A�{A�{A�{A�{A�{A�{B
=B

=B
=B
=B"
=B*
=B2
=B:
=BB
=BJ
=BR
=BZ
=Bb
=Bj
=Br
=Bz
=B�B�B�B�B�B�B�B�B�B�B���B�B�B�B�B�B�B�B�B�B�B�B���B���B�B�B�B�B�B�B�B�C ��C��C��C��C��C
��C��C��C��C��C��C��C��C��C��C��C ��C"��C$��C&��C(��C*��C,��C.��C0��C2��C4��C6��C8��C:��C<��C>��C@��CB��CD��CF��CH��CJ��CL��CN��CP��CR��CT��CV��CX��CZ��C\��C^��C`��Cb��Cd��Cf��Ch��Cj��Cl��Cn��Cp��Cr��Ct��Cv��Cx��Cz��C|��C~��C�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�4{C�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHD  �D ��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D	 �D	��D
 �D
��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D  �D ��D! �D!��D" �D"��D# �D#��D$ �D$��D% �D%��D& �D&��D' �D'��D( �D(��D) �D)��D* �D*��D+>D+��D, �D,��D- �D-��D. �D.��D/ �D/��D0 �D0��D1 �D1��D2 �D2��D3 �D3��D4 �D4��D5 �D5��D6 �D6��D7 �D7��D8 �D8��D9 �D9��D: �D:��D; �D;��D< �D<��D= �D=��D> �D>��D? �D?��D@ �D@��DA �DA��DB �DB��DC �DC��DD �DD��DE �DE��DF �DF��DG �DG��DH �DH��DI �DI��DJ �DJ��DK �DK��DL �DL��DM �DM��DN �DN��DO �DO��DP �DP��DQ �DQ��DR �DR��DS �DS��DT �DT��DU �DU��DV �DV��DW �DW��DX �DX��DY �DY��DZ �DZ��D[ �D[��D\ �D\��D] �D]��D^ �D^��D_ �D_��D` �D`��Da �Da��Db �Db��Dc �Dc��Dd �Dd��De �De��Df �Df��Dg �Dg��Dh �Dh��Di �Di��Dj �Dj��Dk �Dk��Dl �Dl��Dm �Dm��Dn �Dn��Do �Do��Dp �Dp��Dq �Dq��Dr �Dr��Ds �Ds��Dt �Dt��Dy��D�&�D�`RD���D��RD� RD�i�D���D��D� RD�Y�D���D���D��D�S�Dڙ�D��RD�D�S�D�}D��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�C�A�I�A�K�A�M�A�K�A�O�A�Q�A�Q�A�Q�A�S�A�XA�\)A�VA�\)A�ZA�\)A�^5A�\)A�\)A�ZA�\)A�^5A�`BA�bNA�bNA�bNA�dZA�ffA�ffA�ffA�hsA�jA�l�A�l�A�p�A�t�A�z�A�~�A̓A�hsA�E�Ạ�A�Q�A�"�A��A��;A���A�5?AʾwA���A�"�AȍPA�XA�+A�p�A�oA���A�p�A�ffA���A���A���A��TA���A���A��A���A��A�bA��A�l�A���A��`A�
=A���A��jA�&�A�-A�|�A�M�A�jA�A�n�A��A��A��A���A�C�A��/A��A��A�C�A���A���A�A���A��A�dZA��uA��\A|�+Au�
At  Ar�Ap�Al�Ai%AeƨAd-A`bNA^�AX�AUl�AR��AQ+APA�AM�AIAIVAH�\AC�A@M�A?��A?%A>~�A<��A;�7A:�A9/A7�;A61A4�+A2$�A0��A.��A-�
A-dZA-�A,�A,I�A*��A)�A)�A(�A'�^A&Q�A$�A$��A$-A#��A#G�A"1'A!�7A ffA �A�Av�A��A��A5?A�mA��A"�AQ�AXAȴA=qA��A��A;dA�DA��A�`A�A��A�A�TA�9A��A�A�DA
�A	��A9XAG�A��A�-A�mA��A�+A�hAXA;dA�A��Az�A�A��AS�A ��A ��A v�@���@��^@�V@�Z@��@���@�~�@��@��#@��@�O�@�%@���@��@��/@���@�z�@�bN@��w@���@�@��@�V@��@��/@�z�@�C�@��@�|�@�t�@��H@�n�@�@�X@�M�@�@�!@���@ꗍ@�h@�w@�|�@�^5@���@�33@�1'@�x�@��@��@�@�F@��m@��@�%@�z�@���@���@�Q�@� �@��@��@��@�ȴ@�E�@�{@���@�hs@���@�1@���@�33@�!@�=q@�@��@� �@�@�~�@�$�@ݑh@���@܋D@�A�@��;@۝�@��@��@٩�@�`B@�r�@���@�;d@֏\@�-@թ�@�%@� �@�dZ@�K�@�o@҇+@�ff@�ff@�ff@�V@�-@��#@щ7@�Ĝ@�(�@ϕ�@�@�5?@���@Ͳ-@�/@��`@�  @�l�@�"�@ʰ!@ɲ-@��@�I�@Ǖ�@�;d@�+@�@��y@Ɵ�@�-@��@��@�@���@ŉ7@�&�@ēu@���@å�@�C�@�n�@���@�&�@�%@���@��D@�A�@���@�K�@��@���@�^5@��#@���@���@�hs@�G�@�V@���@�A�@�"�@��@���@���@��+@�=q@�-@��^@�X@�%@��u@�|�@��@���@�~�@�E�@�5?@�{@��@�@�&�@�z�@�Z@�I�@�9X@� �@��
@�S�@��R@��7@���@�I�@��@���@�C�@��y@�^5@�=q@�M�@��@�@�hs@�7L@��@���@�9X@��F@�\)@��@�
=@�@�@���@�{@�G�@�%@��j@��@�z�@���@��w@��@�l�@�K�@�o@���@��\@���@���@�~�@�=q@���@�x�@�7L@��@��@��@���@��P@�C�@��@�@���@���@�E�@�J@���@��@�`B@�G�@��@��j@�9X@�\)@��@��R@���@��@��@���@�9X@��w@��@�l�@��@��\@�V@�@���@�p�@��@�V@��`@��D@��m@�dZ@��y@��R@���@�n�@�@���@�X@��@��@���@��@�;d@���@|�/@w��@j�@b�!@Z�!@P�9@G�;@?|�@4I�@-�-@'��@!X@@(�@5?@G�@�+11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�C�A�I�A�K�A�M�A�K�A�O�A�Q�A�Q�A�Q�A�S�A�XA�\)A�VA�\)A�ZA�\)A�^5A�\)A�\)A�ZA�\)A�^5A�`BA�bNA�bNA�bNA�dZA�ffA�ffA�ffA�hsA�jA�l�A�l�A�p�A�t�A�z�A�~�A̓A�hsA�E�Ạ�A�Q�A�"�A��A��;A���A�5?AʾwA���A�"�AȍPA�XA�+A�p�A�oA���A�p�A�ffA���A���A���A��TA���A���A��A���A��A�bA��A�l�A���A��`A�
=A���A��jA�&�A�-A�|�A�M�A�jA�A�n�A��A��A��A���A�C�A��/A��A��A�C�A���A���A�A���A��A�dZA��uA��\A|�+Au�
At  Ar�Ap�Al�Ai%AeƨAd-A`bNA^�AX�AUl�AR��AQ+APA�AM�AIAIVAH�\AC�A@M�A?��A?%A>~�A<��A;�7A:�A9/A7�;A61A4�+A2$�A0��A.��A-�
A-dZA-�A,�A,I�A*��A)�A)�A(�A'�^A&Q�A$�A$��A$-A#��A#G�A"1'A!�7A ffA �A�Av�A��A��A5?A�mA��A"�AQ�AXAȴA=qA��A��A;dA�DA��A�`A�A��A�A�TA�9A��A�A�DA
�A	��A9XAG�A��A�-A�mA��A�+A�hAXA;dA�A��Az�A�A��AS�A ��A ��A v�@���@��^@�V@�Z@��@���@�~�@��@��#@��@�O�@�%@���@��@��/@���@�z�@�bN@��w@���@�@��@�V@��@��/@�z�@�C�@��@�|�@�t�@��H@�n�@�@�X@�M�@�@�!@���@ꗍ@�h@�w@�|�@�^5@���@�33@�1'@�x�@��@��@�@�F@��m@��@�%@�z�@���@���@�Q�@� �@��@��@��@�ȴ@�E�@�{@���@�hs@���@�1@���@�33@�!@�=q@�@��@� �@�@�~�@�$�@ݑh@���@܋D@�A�@��;@۝�@��@��@٩�@�`B@�r�@���@�;d@֏\@�-@թ�@�%@� �@�dZ@�K�@�o@҇+@�ff@�ff@�ff@�V@�-@��#@щ7@�Ĝ@�(�@ϕ�@�@�5?@���@Ͳ-@�/@��`@�  @�l�@�"�@ʰ!@ɲ-@��@�I�@Ǖ�@�;d@�+@�@��y@Ɵ�@�-@��@��@�@���@ŉ7@�&�@ēu@���@å�@�C�@�n�@���@�&�@�%@���@��D@�A�@���@�K�@��@���@�^5@��#@���@���@�hs@�G�@�V@���@�A�@�"�@��@���@���@��+@�=q@�-@��^@�X@�%@��u@�|�@��@���@�~�@�E�@�5?@�{@��@�@�&�@�z�@�Z@�I�@�9X@� �@��
@�S�@��R@��7@���@�I�@��@���@�C�@��y@�^5@�=q@�M�@��@�@�hs@�7L@��@���@�9X@��F@�\)@��@�
=@�@�@���@�{@�G�@�%@��j@��@�z�@���@��w@��@�l�@�K�@�o@���@��\@���@���@�~�@�=q@���@�x�@�7L@��@��@��@���@��P@�C�@��@�@���@���@�E�@�J@���@��@�`B@�G�@��@��j@�9X@�\)@��@��R@���@��@��@���@�9X@��w@��@�l�@��@��\@�V@�@���@�p�@��@�V@��`@��D@��m@�dZ@��y@��R@���@�n�@�@���@�X@��@��@���@��@�;d@���@|�/@w��@j�@b�!@Z�!@P�9@G�;@?|�@4I�@-�-@'��@!X@@(�@5?@G�@�+11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
�5B
�5B
�;B
�;B
�;B
�;B
�;B
�;B
�5B
�;B
�;B
�BB
�;B
�BB
�;B
�;B
�;B
�;B
�5B
�5B
�5B
�;B
�;B
�;B
�;B
�;B
�;B
�;B
�;B
�;B
�;B
�BB
�HB
�HB
�TB
�TB
�mB
�yB
�BaHB� B�%B�B�7B�=B�7B�DB�JB�Bx�Bz�B�hB��B�B�B�B49BM�BYB\)B^5BaHBe`Bq�Bm�Bn�Bn�Bl�BiyBe`BcTBbNB`BB\)BVB?}B�B �BJB�B:^BS�BR�BO�B+B�3B8RB
��B
�B
�'B
�1B
��B
��B
�+B
n�B
R�B
@�B
/B
$�B
�B	�B	��B	B	�LB	�B	�uB	|�B	iyB	]/B	J�B	6FB	�B	DB	B	B��B��B�B�B�B�B�B�B�B�B�B�B�B�B�sB�B�yB�B�yB�mB�B��B��B��B��B	1B	bB	{B	VB		7B	JB	{B	�B	%�B	7LB	9XB	O�B	T�B	[#B	[#B	\)B	]/B	^5B	bNB	gmB	k�B	n�B	r�B	x�B	~�B	�B	�B	�B	�B	�B	�B	�B	�B	�=B	�1B	�%B	�B	z�B	k�B	jB	ffB	W
B	L�B	P�B	>wB	6FB	6FB	@�B	M�B	Q�B	L�B	S�B	W
B	XB	W
B	T�B	S�B	Q�B	P�B	P�B	Q�B	R�B	R�B	VB	VB	W
B	W
B	YB	[#B	^5B	_;B	`BB	`BB	aHB	aHB	aHB	aHB	cTB	dZB	cTB	e`B	e`B	dZB	cTB	dZB	gmB	iyB	k�B	iyB	gmB	dZB	gmB	ffB	dZB	e`B	iyB	v�B	t�B	r�B	t�B	s�B	o�B	k�B	l�B	m�B	v�B	~�B	�=B	��B	��B	��B	��B	��B	��B	�B	�FB	�RB	�wB	ŢB	ǮB	ǮB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�#B	�5B	�;B	�BB	�HB	�HB	�HB	�HB	�HB	�TB	�NB	�NB	�TB	�TB	�TB	�TB	�TB	�ZB	�ZB	�ZB	�fB	�`B	�`B	�fB	�fB	�fB	�fB	�fB	�`B	�`B	�`B	�fB	�fB	�fB	�fB	�fB	�fB	�`B	�`B	�`B	�ZB	�`B	�`B	�`B	�fB	�ZB	�ZB	�`B	�`B	�`B	�`B	�fB	�fB	�mB	�fB	�fB	�fB	�fB	�fB	�mB	�fB	�sB	�B	�B	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
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
1B
	7B
	7B
	7B
	7B
	7B
	7B

=B

=B

=B

=B
DB
DB
DB
DB
DB
DB
DB
JB
JB
JB
JB
PB
PB
PB
PB
VB
VB
VB
\B
\B
\B
\B
\B
bB
bB
bB
bB
bB
oB
oB
uB
uB
uB
{B
�B
�B
�B
�B
�B
�B
 �B
&�B
/B
5?B
7LB
?}B
D�B
H�B
K�B
N�B
T�B
]/B
aHB
gmB
m�B
p�B
q�B
w�B
{�B
~�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B
�B
�B
�B
�B
�B
�B
�B
�B
�
B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�!B
�B
�+B
�-B
�AB
�PB
�bBaB�B��B��B�B�B�B�B� B��Bx�Bz�B�?BϴB�[B�BjB4BM�BX�B[�B^BaBe2Bq�BmdBnkBnlBl^BiJBe3Bc(BbB`B[�BU�B?NBqB �BBYB:,BS�BR�BO�B*�B�B8"B
��B
��B
��B
��B
��B
�qB
��B
ndB
R�B
@QB
.�B
$�B
VB	�JB	ʍB	�[B	�B	��B	�@B	|�B	iEB	\�B	J�B	6B	\B	B	�B	 �B��B��B�}B�lB�NB�OB�QB�RB�SB�QB�SB�VB�EB�FB�;B�EB�=B�PB�=B�2B�JB��B��B��B��B	�B	(B	@B	B	�B	B	@B	]B	%�B	7B	9B	O�B	T�B	Z�B	Z�B	[�B	\�B	]�B	bB	g4B	kJB	nZB	rsB	x�B	~�B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	z�B	kHB	jAB	f(B	V�B	L�B	P�B	>:B	6B	6B	@GB	M�B	Q�B	L�B	S�B	V�B	W�B	V�B	T�B	S�B	Q�B	P�B	P�B	Q�B	R�B	R�B	U�B	U�B	V�B	V�B	X�B	Z�B	]�B	^�B	` B	`B	a
B	aB	aB	a	B	cB	dB	cB	e"B	e B	dB	cB	dB	g-B	i8B	kDB	i9B	g/B	dB	g-B	f(B	dB	e B	i9B	v�B	t{B	roB	t|B	sxB	o\B	kGB	lMB	mSB	v�B	~�B	��B	�YB	��B	��B	�sB	��B	��B	��B	�B	�B	�5B	�dB	�nB	�lB	�pB	�rB	ʁB	͑B	͓B	͓B	ΛB	ΚB	ΛB	ϟB	ФB	УB	ѮB	ѭB	ѫB	ԾB	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�#B	�B	�!B	�(B	�#B	�#B	�#B	�#B	�B	�B	�B	�'B	�%B	�%B	�%B	�'B	�&B	�B	�!B	� B	�B	�!B	�B	�B	�%B	�B	�B	�B	�B	�B	�B	�$B	�(B	�,B	�%B	�%B	�#B	�$B	�%B	�,B	�'B	�2B	�>B	�?B	�7B	�7B	�=B	�LB	�RB	�JB	�AB	�LB	�QB	�VB	�VB	�PB	�RB	�UB	�\B	�jB	�tB	�sB	�tB	�nB	�lB	�uB	�|B	�|B	�|B	��B	�}B	�|B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
 �B
 �B
 �B	��B	��B
 �B
 �B
 �B
 �B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
�B
�B
�B
�B
�B
�B
	�B
	�B
	�B
	�B
B
B
 B
 B
B
B
B
B
B
B
	B
B
B
B
B
B
B
B
B
B
B
B
B
B
!B
B
B
B
.B
,B
2B
3B
3B
:B
<B
>B
=B
>B
KB
XB
 �B
&�B
.�B
4�B
7
B
?9B
DYB
HqB
K�B
N�B
T�B
\�B
aB
g,B
mNB
paB
qjB
w�B
{�B
~�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.51 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                                                                                                             201904051007592019040510075920190405100759  AO  ARCAADJP                                                                    20181121125953    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181121125953  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181121125953  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20190405100759  IP                  G�O�G�O�G�O�                