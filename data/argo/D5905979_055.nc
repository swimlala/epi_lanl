CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2020-06-19T17:09:06Z creation      
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20200619170906  20220204114416  5905979 UW, Argo                                                        STEPHEN RISER                                                   PRES            TEMP            PSAL               7A   AO  7662                            2C  D   APEX                            8312                            080318                          846 @آ�����1   @آ�ww��@6?|�h�c����+1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    7A   B   B   @���@�  @���A   A@  A`  A���A�  A�  A�  A�  A�  A�  A�  B   B  B  B��B   B'��B/��B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD�fD  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&y�D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4�fD5  D5� D6  D6� D6��D7y�D7��D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@�fDA  DA� DB  DB� DC  DC� DD  DD� DE  DE�fDF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK�fDL  DL� DM  DM� DN  DN� DO  DO� DP  DP�fDQfDQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De�fDf  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt�fDy��D��D�D�D��3D�� D��D�C�D�|)D���D�
=D�i�D���D��)D��D�XRDښ=D��\D��D�eqD�D��f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��R@��@��A�\A>�\A^�\A�{A�G�A�G�A�G�A�G�A�G�A�G�A�G�A�G�B��B��B=qB��B'=qB/=qB7��B?��BG��BO��BW��B_��Bg��Bo��Bw��B��B���B���B�B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B۞�B���B���B���B���B���B���B���B���B���C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C�HC��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{D z=D �=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=D	z=D	�=D
z=D
�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D �D��D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=D z=D �=D!z=D!�=D"z=D"�=D#z=D#�=D$z=D$�=D%z=D%�=D&s�D&�=D'z=D'�=D(z=D(�=D)z=D)�=D*z=D*�=D+z=D+�=D,z=D,�=D-z=D-�=D.z=D.�=D/z=D/�=D0z=D0�=D1z=D1�=D2z=D2�=D3z=D3�=D4��D4�=D5z=D5�=D6z=D6��D7s�D7��D8z=D8�=D9z=D9�=D:z=D:�=D;z=D;�=D<z=D<�=D=z=D=�=D>z=D>�=D?z=D?�=D@��D@�=DAz=DA�=DBz=DB�=DCz=DC�=DDz=DD�=DE��DE�=DFz=DF�=DGz=DG�=DHz=DH�=DIz=DI�=DJz=DJ�=DK��DK�=DLz=DL�=DMz=DM�=DNz=DN�=DOz=DO�=DP��DQ �DQz=DQ�=DRz=DR�=DSz=DS�=DTz=DT�=DUz=DU�=DVz=DV�=DWz=DW�=DXz=DX�=DYz=DY�=DZz=DZ�=D[z=D[�=D\z=D\�=D]z=D]�=D^z=D^�=D_z=D_�=D`z=D`�=Daz=Da�=Dbz=Db�=Dcz=Dc�=Ddz=Dd�=De��De�=Dfz=Df�=Dgz=Dg�=Dhz=Dh�=Diz=Di�=Djz=Dj�=Dkz=Dk�=Dlz=Dl�=Dmz=Dm�=Dnz=Dn�=Doz=Do�=Dpz=Dp�=Dqz=Dq�=Drz=Dr�=Dsz=Ds�=Dtz=Dt��Dy��D��D�A�D��RD��D��D�@�D�yHD���D�\D�f�D���D��HD��D�UqDڗ\D��{D��D�b�D��D�Ӆ111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�p�A�v�A�|�A�AuA�AuAuAuA�A�A�A�A�AuA�A�A�A�A�A¡�A£�A¥�A¥�A¥�A£�A§�A©�A©�A©�A©�A©�A¡�A�A�A�A�A�AuA�XA�&�A�1A���A�/A�r�A�ffA�%A�v�A�v�A�VA�n�A��DA�z�A���A�`BA���A��A�A�A�  A�ĜA�A�A�A�v�A�1'A��
A��A�7LA��-A��A�G�A�z�A��A��;A��DA�%A��yA��!A�&�A��PA���A�VA�oA���A��A�{A���A�{A��A��!A���A�n�A�?}A��uA�~�A�z�A���A�bA�~�A�&�A���A��HA�-A�=qA�+A��HA��FA��A���A�S�A��PA��A�VA}"�A{�Az�Az��AzZAy�wAy�Ay?}Ax~�Aw��Au��At�RAr~�Ap��Ao�TAm��Ait�Af�yAe��Ac&�AaXA`bNA^9XA[&�AY�AXVAV=qAR=qAPE�AO�PAN��AN9XAM��AM�PAL�AK��AIl�AG�hAEhsADJAB�+AAoA@ZA?&�A=�wA;�A7�PA6r�A5��A4n�A1�-A.�DA-��A-x�A,�A(A�A%�7A$v�A$I�A#�hA#?}A"ȴA!��A {A�A��A�/A�A��AhsAȴAM�A{A��A`BAdZAoA�AA�AQ�A��A��AC�A��A=qA��AA�^A��A�9Ax�AVA
�DA
JA	��A	�A~�A(�A"�A$�A�A��A�jAZA�PA �@���@�r�@�5?@�A�@��w@�"�@�%@�\)@�@�7L@@�=q@��@�Z@��@ꗍ@�@��@�I�@��@�o@�&�@㕁@�ff@ߍP@��@�\)@�J@�hs@�V@��@أ�@�1'@�ƨ@�dZ@�ȴ@��@�@��@�Ĝ@ϥ�@�5?@�"�@�"�@�$�@��#@ŉ7@�G�@���@���@Ĵ9@�j@��;@�33@��#@�n�@�b@�ȴ@��\@�^5@��T@���@�hs@�?}@��@���@�"�@��H@�ȴ@�{@�/@�I�@��m@���@�ƨ@��w@�ƨ@���@��@�"�@��!@���@��-@�X@��/@�b@�(�@��D@�1'@��@���@��@�|�@�ȴ@�=q@�1@�{@�p�@�hs@�O�@�/@���@�r�@���@���@��/@�z�@��m@�\)@��@��w@��y@�$�@��@�5?@�ff@�~�@���@��R@���@���@�V@�J@��#@���@���@���@�@��^@��-@���@���@���@��7@��h@�p�@�V@�Ĝ@�Ĝ@��j@���@�j@�A�@��@�  @�ƨ@�@��\@��-@���@��@�G�@�V@�j@�(�@�b@�  @���@���@���@�ff@��-@���@�z�@��@�dZ@���@�$�@�?}@���@��@�A�@� �@�b@�(�@�z�@�1'@��m@���@��w@��F@��F@���@�  @�b@��@� �@�1@��
@���@���@�dZ@�o@�ȴ@�V@�=q@��@���@�@�/@���@�Ĝ@��D@�I�@�9X@�A�@�1'@��@��@��!@�=q@��@��@�b@��m@��;@��w@���@�S�@��H@�ȴ@���@���@���@�~�@�v�@�n�@�n�@�ff@�ff@�ff@�M�@�$�@�J@��#@�G�@��@��@��D@�j@�A�@�1'@�(�@�b@��;@�|�@�ȴ@��!@���@��\@�ff@�5?@��#@�hs@�&�@��@��@�r�@�;@K�@�@~ȴ@~�+@}��@|��@{��@{��@{dZ@{S�@{"�@y#�@s��@jR�@]��@X~(@R��@O~�@J��@C�q@<��@8��@0ѷ@*҉@#��@$@J�@��@ \@	V@�$@C�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A�p�A�v�A�|�A�AuA�AuAuAuA�A�A�A�A�AuA�A�A�A�A�A¡�A£�A¥�A¥�A¥�A£�A§�A©�A©�A©�A©�A©�A¡�A�A�A�A�A�AuA�XA�&�A�1A���A�/A�r�A�ffA�%A�v�A�v�A�VA�n�A��DA�z�A���A�`BA���A��A�A�A�  A�ĜA�A�A�A�v�A�1'A��
A��A�7LA��-A��A�G�A�z�A��A��;A��DA�%A��yA��!A�&�A��PA���A�VA�oA���A��A�{A���A�{A��A��!A���A�n�A�?}A��uA�~�A�z�A���A�bA�~�A�&�A���A��HA�-A�=qA�+A��HA��FA��A���A�S�A��PA��A�VA}"�A{�Az�Az��AzZAy�wAy�Ay?}Ax~�Aw��Au��At�RAr~�Ap��Ao�TAm��Ait�Af�yAe��Ac&�AaXA`bNA^9XA[&�AY�AXVAV=qAR=qAPE�AO�PAN��AN9XAM��AM�PAL�AK��AIl�AG�hAEhsADJAB�+AAoA@ZA?&�A=�wA;�A7�PA6r�A5��A4n�A1�-A.�DA-��A-x�A,�A(A�A%�7A$v�A$I�A#�hA#?}A"ȴA!��A {A�A��A�/A�A��AhsAȴAM�A{A��A`BAdZAoA�AA�AQ�A��A��AC�A��A=qA��AA�^A��A�9Ax�AVA
�DA
JA	��A	�A~�A(�A"�A$�A�A��A�jAZA�PA �@���@�r�@�5?@�A�@��w@�"�@�%@�\)@�@�7L@@�=q@��@�Z@��@ꗍ@�@��@�I�@��@�o@�&�@㕁@�ff@ߍP@��@�\)@�J@�hs@�V@��@أ�@�1'@�ƨ@�dZ@�ȴ@��@�@��@�Ĝ@ϥ�@�5?@�"�@�"�@�$�@��#@ŉ7@�G�@���@���@Ĵ9@�j@��;@�33@��#@�n�@�b@�ȴ@��\@�^5@��T@���@�hs@�?}@��@���@�"�@��H@�ȴ@�{@�/@�I�@��m@���@�ƨ@��w@�ƨ@���@��@�"�@��!@���@��-@�X@��/@�b@�(�@��D@�1'@��@���@��@�|�@�ȴ@�=q@�1@�{@�p�@�hs@�O�@�/@���@�r�@���@���@��/@�z�@��m@�\)@��@��w@��y@�$�@��@�5?@�ff@�~�@���@��R@���@���@�V@�J@��#@���@���@���@�@��^@��-@���@���@���@��7@��h@�p�@�V@�Ĝ@�Ĝ@��j@���@�j@�A�@��@�  @�ƨ@�@��\@��-@���@��@�G�@�V@�j@�(�@�b@�  @���@���@���@�ff@��-@���@�z�@��@�dZ@���@�$�@�?}@���@��@�A�@� �@�b@�(�@�z�@�1'@��m@���@��w@��F@��F@���@�  @�b@��@� �@�1@��
@���@���@�dZ@�o@�ȴ@�V@�=q@��@���@�@�/@���@�Ĝ@��D@�I�@�9X@�A�@�1'@��@��@��!@�=q@��@��@�b@��m@��;@��w@���@�S�@��H@�ȴ@���@���@���@�~�@�v�@�n�@�n�@�ff@�ff@�ff@�M�@�$�@�J@��#@�G�@��@��@��D@�j@�A�@�1'@�(�@�b@��;@�|�@�ȴ@��!@���@��\@�ff@�5?@��#@�hs@�&�@��@��@�r�@�;@K�@�@~ȴ@~�+@}��@|��@{��@{��@{dZ@{S�G�O�@y#�@s��@jR�@]��@X~(@R��@O~�@J��@C�q@<��@8��@0ѷ@*҉@#��@$@J�@��@ \@	V@�$@C�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBs�Bs�Bs�Br�Br�Br�Br�Br�Br�Br�Br�Br�Br�Bs�Bs�Br�Br�Br�Br�Bs�Bs�Bs�Bs�Bs�Bs�Bt�Bs�Bs�Bs�Bt�Bt�Bt�Bu�Bv�Bv�Bv�Bw�Bw�By�B�%B�uB��B�RB�NB�B<jBF�BM�BK�B-BbBDB�B�B�B'�B-B-B2-B8RB7LB8RB:^B9XB9XB8RB8RB8RB6FB5?B33B-B%�B&�B(�B$�B!�B �BbB��B�B�B�sB�fB�#B��B�jB��B}�B_;BN�BM�BC�B"�B	7B+B
��B
��B
�B
��B
�+B
z�B
^5B
I�B
C�B
?}B
5?B
�B
uB
+B	��B	�`B	��B	ȴB	ÖB	B	ÖB	��B	��B	�wB	�^B	�FB	�'B	�B	��B	��B	��B	��B	�JB	~�B	s�B	bNB	VB	N�B	J�B	9XB	33B	+B	#�B	�B	%B	B	B	  B��B��B��B��B�B�NB�B��BƨB�LB�'B��B��B��B�B}�Bz�Br�Bk�BaHB\)BZBXBP�BM�BI�BK�BO�BQ�BQ�BO�BP�BO�BN�BN�BO�BP�BN�BQ�BM�BM�BL�BL�BK�BJ�BH�BF�BF�BB�B@�B>wB<jB;dB:^B9XB8RB6FB5?B5?B33B2-B2-B1'B1'B0!B1'B.B-B-B,B+B)�B)�B)�B'�B'�B%�B'�B&�B$�B%�B&�B&�B&�B(�B-B.B.B33B7LB>wBJ�BJ�BG�BH�BR�BcTB[#BXBM�BM�BR�BT�BVBYB\)B_;B`BB`BB`BB`BB`BBhsBk�Bl�Bm�Bm�Bo�Bu�Bu�Bu�Bv�Bw�Bw�Bw�Bx�Bx�Bx�Bx�By�B�B�DB�\B�VB�\B�hB�hB�hB�hB�oB�uB�uB�oB�hB�{B��B��B��B��B��B��B��B��B��B��B�!B�3B�9B�?B�RB�jB�}BǮB��B��B��B�TB�B�B��B	B	\B	oB	uB	{B	{B	{B	�B	�B	�B	�B	"�B	"�B	#�B	'�B	&�B	(�B	,B	.B	/B	1'B	2-B	33B	5?B	8RB	B�B	I�B	L�B	O�B	O�B	O�B	O�B	P�B	P�B	P�B	Q�B	Q�B	Q�B	S�B	R�B	VB	\)B	_;B	_;B	`BB	aHB	cTB	e`B	gmB	gmB	jB	q�B	u�B	~�B	~�B	� B	�B	�B	�1B	�7B	�=B	�=B	�DB	�PB	�PB	�VB	�bB	�hB	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�-B	�FB	�LB	�RB	�XB	�dB	�dB	�jB	�wB	�}B	�wB	�qB	�jB	�jB	�dB	�dB	�}B	�}B	�}B	��B	B	B	B	ÖB	ĜB	ŢB	ŢB	ƨB	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�#B	�#B	�#B	�)B	�/B	�/B	�;B	�;B	�;B	�BB	�TB	�ZB	�ZB	�ZB	�ZB	�`B	�`B	�`B	�`B	�fB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
[B

rB
{B
"�B
&�B
+B
0oB
5�B
?cB
CB
G�B
P�B
W�B
^5B
fB
kQB
oOB
t�B
y$B
~�B
��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  Bk�Bk�Bk�Bj�Bj�Bj�Bj�Bj�Bj�Bj�Bj�Bj�Bj�Bk�Bk�Bj�Bj�Bj�Bj�Bk�Bk�Bk�Bk�Bk�Bk�Bl�Bk�Bk�Bk�Bl�Bl�Bl�Bm�Bn�Bn�Bn�Bo�Bo�Bq�B~B�SB��B�.B�(B�B4>B>|BE�BC�B$�B:BBxB�B�B�B$�B$�B*B0*B/$B0*B26B10B10B0*B0*B0*B.B-B+B$�B�B�B �B�B�B�B>B�B�B�iB�RB�EB�B��B�LB��Bu�BW#BF�BE�B;�B�B&B
�B
��B
��B
�B
��B
#B
r�B
V0B
A�B
;�B
7zB
-=B
�B
uB	�,B	��B	�dB	��B	��B	��B	��B	��B	��B	��B	�~B	�eB	�MB	�/B	�
B	��B	��B	��B	��B	�UB	wB	k�B	Z\B	NB	F�B	B�B	1jB	+EB	#B	�B	�B�;B�(B�"B�B�B�B�B��B�B�gB�B��B��B�hB�DB�B��B��B|9BvBsBj�Bc�BYlBTMBRABP4BI
BE�BA�BC�BHBJBJBHBIBHBF�BF�BHBIBG BJBE�BE�BD�BD�BC�BB�B@�B>�B>�B:�B8�B6�B4�B3�B2�B1�B0{B.pB-iB-iB+]B*WB*WB)RB)RB(LB)RB&?B%9B%:B$4B#.B"(B"(B"(B B BB BB
BBBBB!#B%;B&AB&AB+`B/yB6�BB�BB�B?�B@�BKB[~BSNBP;BE�BE�BKBM*BN0BQCBTUBWfBXmBXmBXmBXnBXnB`�Bc�Bd�Be�Be�Bg�Bm�Bm�Bm�Bn�Bo�Bo�Bo�Bq Bq Bq Bq BrB}JB�oB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�%B�JB�\B�bB�hB�{B��B��B��B��B�B�%B�zB�B��B� B�7B	�B	
�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	 B	B	!B	$+B	&7B	'>B	)JB	*PB	+VB	-aB	0tB	:�B	A�B	D�B	H B	H B	H B	H B	IB	IB	IB	JB	JB	JB	LB	KB	N$B	TIB	W[B	W[B	XbB	YhB	[tB	]�B	_�B	_�B	b�B	i�B	m�B	wB	wB	xB	z*B	|7B	�OB	�UB	�[B	�[B	�bB	�nB	�nB	�tB	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�0B	�IB	�bB	�hB	�mB	�sB	�B	�B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	� B	�B	�B	�B	�B	�B	�*B	�*B	�1B	�1B	�1B	�7B	�=B	�=B	�=B	�CB	�IB	�IB	�UB	�UB	�UB	�\B	�nB	�tB	�tB	�tB	�tB	�zB	�zB	�zB	�zB	�B	��B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�G�O�B	�sB
�B
�B
B
�B
#B
(�B
.B
7yB
;(B
?�B
H�B
O�B
VJB
^-B
cfB
gdB
mB
q9B
v�B
y�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.09 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9998(+/-0.0001), vertically averaged dS =-0.008(+/-0.003) in PSS-78.                                                                                                                                                                                       Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                      202202041144162022020411441620220204114416  AO  ARCAADJP                                                                    20200619170906    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20200619170906  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20200619170906  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20220204114416  IP                  G�O�G�O�G�O�                